import unittest 
from prueba6 import *
class testTriadaPitagorica(unittest.TestCase):
    def testTridadPitagoricaFalse(self) :
        return self.assertFalse(triadaPitagorica(1,1,1))
    def testTridadPitagoricaTrue(self) :
        return self.assertTrue(triadaPitagorica(3,4,5))

class testEsMultipoDe(unittest.TestCase):
     def testEsMultipoDeTrue(self):
          return self.assertTrue(esMultiploDe(4,2))
     def testEsMultipoDeFalse(self):
        return self.assertFalse(esMultiploDe(2,4))

class testDobleSiEsPar(unittest.TestCase):
      def testImpar(self):
            return self.assertEqual(dobleSiEsPar(1),1)
      def testPar(self):
            return self.assertEqual(dobleSiEsPar(4),8)

class testFarenheitACelcius(unittest.TestCase):
    def testCeroCelsius(self):
        return self.assertEqual(fahrenheitACelsius(32),0)
    def testHervirAgua(self):
        return self.assertEqual(fahrenheitACelsius(212),100)    
    def testCeroFarenheit(self):
        return self.assertEqual(fahrenheitACelsius(0),-17.77777777777778)
    def testNegativoFarenheit(self):
            return self.assertEqual(fahrenheitACelsius(-31),-35)

if __name__ == '__main__':
    unittest.main(verbosity=2)