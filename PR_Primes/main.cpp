#include <omp.h>
#include <iostream>
#include <cmath>
#include <cstdio>
#include <utility>
#include <cstdlib>
#include <vector>
#include <algorithm>
#include <set>
using namespace std;
#pragma omp declare reduction(merge: std::vector<int> : omp_out.insert(omp_out.end(), omp_in.begin(), omp_in.end()))

#define BLOCKLOW(id, p ,n) ((id) * (n) /(p))
#define BLOCKHIGH(id, p,n) (BLOCKLOW((id) + 1,(p) ,(n)) -1)
#define BLOCKSIZE(id,p,n) (BLOCKHIGH((id),(p),(n)) - BLOCKLOW((id),(p),(n)) + 1)

#define MAX 100000000
#define THREADS 4

// funkcja pomocnicza określająca czy dana liczba z zakresu <2,sqrt(n)> jest liczbą pierwszą
bool is_primary(int n) {
	if (n < 2) return false;
	for (int i = 2; i * i <= n; i++)
		if (n % i == 0) return false;
	return true;
}
//funkcja, przesiewająca wszystkie liczby z tablicy, któe są wielokrotnościami el. dividers
vector<int> sieveByDividers(int min, int max, vector<int> dividers) {
	vector<int> toReturn;
	int localMin = 0;
	bool* tablica = new bool[max - min + 1];
	for (int i = 0; i < max - min + 1; i++) tablica[i] = true;
	for (int div : dividers) {
	// jeżeli nasza liczba jest już w zakresie, to należy ją zwrócić.
		if (div >= min) toReturn.push_back(div);
	//minimum 
		localMin = (min % div != 0) ? min + div - min % div : min;
	// pętla wykreślająca
		for (auto i = localMin; i <= max; i += div) {
			tablica[i - min] = false;
		}
	}
	//pętla szukająca pierwszych z powstałej tablicy.
	for (int i = min; i <= max; i++) {
		if (tablica[i - min]) toReturn.push_back(i);
	}
	return toReturn;
}

template<typename T, typename InputIterator, typename OutputIterator>
void removeDup(InputIterator begin, InputIterator end, OutputIterator res) {
	set<T> temp(begin, end);
	copy(temp.begin(), temp.end(), res);
}

//Zbiór funkcji wykorzystujący dzielenie liczb do wyznaczania liczb pierwszych.
namespace Div {

	//Funkcja zwracająca liczby pierwsze metodą dzielenia.
	vector<int> getSequentialPrimaryNumbers(int min, int max) {
		vector<int> primary;
		double start, end;
		start = omp_get_wtime();
		for (int i = min; i <= max; i++) {
			if (is_primary(i)) primary.push_back(i);
		}
		end = omp_get_wtime();
		std::cout << "Time: " << end - start << endl;
		return primary;
	}

	//naiwna metoda, niepoprawna.
	vector<int> getNaivePrimary( int min, int max) {
		vector<int> primary;
		int th = omp_get_max_threads();
		omp_set_num_threads(THREADS);
#pragma omp parallel
		{

#pragma omp for
			for (int i = min; i <= max; i++) {
				if (is_primary(i)) {
					primary.push_back(i);
				}
			}

		}
		return primary;
	}

	//Najszybsza metoda równoległa. bez sortowania
	vector<int> getBestPrimary( int min, int max) {
		vector<int> primary;
		double start, end;
		
		omp_set_num_threads(THREADS);
		start = omp_get_wtime();
#pragma omp parallel
		{
			vector<int> myPrimarys;
#pragma omp for
			for (int i = min; i <= max; i++) {
				if (is_primary(i)) {
					myPrimarys.push_back(i);
				}
			}

#pragma omp critical
			primary.insert(primary.end(), myPrimarys.begin(), myPrimarys.end());
		}
		end = omp_get_wtime();

	
		std::cout << "time: " << end - start << endl;
		return primary;
	}

	//trochę wolniejsza wersja naszybszej, ponieważ uzyskujemy posortowany vektor. Dodatkowo trzeba usunąć powtórzenia.
	vector<int> getBetterPrimary(int min,int max) {
		vector<int> finalPrimarys;
		finalPrimarys.reserve(max - min + 1);
	
		vector<vector<int>> privatePrimarys(THREADS,vector<int>());
		
		for (int i = 0; i < THREADS; i++) {
			privatePrimarys[i].reserve(max - min + 1);
		}
		omp_set_num_threads(THREADS);
		double start = omp_get_wtime();
#pragma omp parallel
		{
#pragma omp for
			for (int i = min; i <= max; i++) {
				if (is_primary(i)) privatePrimarys[omp_get_thread_num()].push_back(i);
			}

			
		}
		for (int i = 0; i < THREADS; i++) {
			finalPrimarys.insert(finalPrimarys.end(), privatePrimarys[i].begin(), privatePrimarys[i].end());
		}
		std::cout << "time: "<< omp_get_wtime() - start << endl;
		return finalPrimarys;
	}
}


//Zbiór funkcji wykorzystujący metodę sita do wyznaczania liczb pierwszych.
namespace Sieve {

	//Funkcja zwracająca liczby pierwsze metodą Sita.
	vector<int> sitoSequential(int min, int max) {
		vector<int> zakres = Div::getSequentialPrimaryNumbers(2, int(sqrt(max)));
		bool* tablica = new bool[max - min + 1];
		for (int i = 0; i < max - min + 1; i++) tablica[i] = true;
		vector<int> primes;
		
		for (int div : zakres) {
			if (div >= min) {
				primes.push_back(div);
			}
			int finalMin = (min % div) ? min + div - min % div : min;
			for (int i = finalMin; i <= max; i += div) {
				tablica[i - min] = false;
			}

		}
		for (int i = min; i <= max; i++) {
			if (tablica[i - min]) primes.push_back(i);
		}
	
		//free(tablica);
		return primes;
	}
	//stio parallel Domain -> liczy od 0 i wykreśla mniejsze niż min.
	vector<int> sitoParDom(int min, int max) {
		

		int threadBlock = (max - min + 1) / THREADS;


		omp_set_num_threads(THREADS);
		vector<int> finalPrimarys;
#pragma omp parallel reduction(merge:finalPrimarys)
		{
			int minBlock = min + threadBlock * omp_get_thread_num();
			int maxBlock = (omp_get_thread_num() == THREADS - 1) ? max : minBlock + threadBlock - 1;
			finalPrimarys = sitoSequential(minBlock, maxBlock);
		}

		return finalPrimarys;
		
	}

	//sito parallel domain best -> najpierw liczymy dzielniki sitem od 0 do sqrt(max) później wykreślamy przez wielokrotności w zakresie min -> max.
	vector<int> sitoParDomBest(int min, int max) {

		vector<int> dzielniki = Div::getBetterPrimary(2, int(sqrt(max)));
		omp_set_num_threads(THREADS);
		vector<int> finalPrimarys;
		int threadBlock = (max - min + 1) / THREADS;
		double start = omp_get_wtime();
#pragma omp parallel reduction(merge: finalPrimarys)
		{
			int minBlock = min + threadBlock * omp_get_thread_num();
			int maxBlock = (omp_get_thread_num() == THREADS - 1) ? max : minBlock + threadBlock - 1;
			finalPrimarys = sieveByDividers(minBlock, maxBlock, dzielniki);
		}
		cout << "Time: " << omp_get_wtime() - start << endl;
		dzielniki.clear();
		return finalPrimarys;

	}

	//Funkcyjne Podejście obliczania sita, wątki dostają całą tablicę boolowską, z któej wykreślają wilokrotności dzielników, które są w vectorze dzielniki, a te są rozdawane wątkom za pomocą 
	// pragma omp for nowait shedule(guided)
	vector<int> sitoParFunBetter(int min, int max) {
		vector<int> finalPrimes;
		//finalPrimes.reserve(max - min + 1);
		vector<int> dzielniki = Div::getSequentialPrimaryNumbers(2, int(sqrt(max)));
		int size = max - min + 1;
		bool* tablica[THREADS];
		for (int i = 0; i < THREADS; i++) {
			tablica[i] = new bool[max - min + 1];
			for (int j = 0; j < size; j++) {
				tablica[i][j] = true;
			}
		}
		omp_set_num_threads(THREADS);
		double start = omp_get_wtime();
#pragma omp parallel
		{
			int minBlock;
#pragma omp for nowait schedule(guided)
			for (int i = 0; i < dzielniki.size(); i++) {
				minBlock = (min % dzielniki[i] != 0) ? min + dzielniki[i] - min % dzielniki[i] : min;

				for (int j = minBlock; j <= max; j += dzielniki[i]) {
					tablica[omp_get_thread_num()][j - min] = false;
				}

			}
		}
		for (int div : dzielniki) {
			if (div >= min) {
				finalPrimes.push_back(div);
				//cout << "dodaje: " << div << endl;
			}
			

		}
		for (int i = min; i <= max; i++) {
			bool isPrime = true;
			for (int j = 0; j < THREADS; j++) {
				if (!tablica[j][i - min]) {
					isPrime = false;
					break;
				}
			}
			if (isPrime) finalPrimes.push_back(i);
		}
		cout << "Time: " << omp_get_wtime() - start << "; Size of vector: " << finalPrimes.size() << endl;
		//free(tablica);

		return finalPrimes;
		
	}

	vector<int> sitoParFunGood(int min, int max) {
		vector<int> dzielniki = Div::getSequentialPrimaryNumbers(2, int(sqrt(max)));
		vector<int> finalPrimes;
		bool* tablica[THREADS];
		for (int i = 0; i < THREADS; i++) {
			tablica[i] = new bool[max - min + 1];
			for (int j = 0; j < max - min + 1; j++) {
				tablica[i][j] = true;
			}
		}

		omp_set_num_threads(THREADS);
		double start = omp_get_wtime();
#pragma omp parallel
		{
			int minBlock;
			for (int i = 0; i < dzielniki.size(); i++) {
				minBlock = (min % dzielniki[i]) ? min + dzielniki[i] - min % dzielniki[i] : min;

#pragma omp for nowait schedule(static)
				for (int j = minBlock; j <= max; j += dzielniki[i]) {
					tablica[omp_get_thread_num()][j - min] = false;
				}
			}
		}
		for (int div : dzielniki) {
			if (div >= min) {
				finalPrimes.push_back(div);
				//cout << "dodaje: " << div << endl;
			}


		}
		for (int i = min; i <= max; i++) {
			bool isPrime = true;
			for (int j = 0; j < THREADS; j++) {
				if (!tablica[j][i - min]) {
					isPrime = false;
					break;
				}
			}
			if (isPrime) finalPrimes.push_back(i);
		}
		cout << "Time: " << omp_get_wtime() - start << "; Size of vector: " << finalPrimes.size() << endl;
		//free(tablica);

		return finalPrimes;
	}





}


	




int main() {

	std::cout << "Hello World" << endl;
	/*vector<int> primarys = Div::getBestPrimary(2, MAX);
	sort(primarys.begin(), primarys.end());
	cout << primarys.size() << endl;
	for (int i = 0; i < 30; i++) {
		cout << primarys.at(i) << ' ';
	}
	cout << endl;
	//for (int p : primarys) cout << p << ' ';
	//primarys.clear();
	vector<int> primarys1 = Div::getBetterPrimary(2, MAX);
	sort(primarys1.begin(), primarys1.end());
	for (int i = 0; i < 30; i++) {
		cout << primarys1.at(i) << ' ';
	}
	cout << endl << primarys1.size() << endl;
	vector<int> prSieve = Sieve::sitoSequential(MAX / 2, MAX);
	for (int i = 0; i < 30; i++) {
		cout << prSieve.at(i) << ' ';
	}
	cout << endl << prSieve.size() << endl;

	vector<int> primarys2 = Sieve::sitoParDom(MAX / 2, MAX);
	for (int i = 0; i < 30; i++) {
		cout << primarys2.at(i) << ' ';
	}
	cout << endl << primarys2.size() << endl; 
	vector<int> primarys3 = Sieve::sitoParFunGood(MAX/2,MAX);
	sort(primarys3.begin(), primarys3.end());
	for (int i = 0; i < 30; i++) {
		cout << primarys3.at(i) << ' ';
	}
	cout << endl << primarys3.size() << endl;
	vector<int> primarys4 = Sieve::sitoParFunBetter(MAX/2, MAX);
	sort(primarys4.begin(), primarys4.end());
	for (int i = 0; i < 30; i++) {
		cout << primarys4.at(i) << ' ';
	}
	cout << endl << primarys4.size() << endl;*/
	vector<int> prSieve = Sieve::sitoSequential(2, MAX/2);
	for (int i = 0; i < 30; i++) {
		cout << prSieve.at(i) << ' ';
	}
	cout << endl << prSieve.size() << endl;
	return 0;
}