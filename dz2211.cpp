#include <iostream>
#include <string>
#include <cstdint>
#include <numeric>
#include <cmath>
#include <limits>

class Rational {
private:
    int64_t numerator_;
    uint64_t denominator_;

    static bool mul_overflow(int64_t a, int64_t b) {
        if (a == 0 || b == 0) return false;
        
        int64_t result = a * b;
        if (a == std::numeric_limits<int64_t>::min() && b == -1) return true;
        if (b == std::numeric_limits<int64_t>::min() && a == -1) return true;
        
        return (b != 0) && (result / b != a);
    }

    static bool add_overflow(int64_t a, int64_t b) {
        int64_t result = a + b;
        
        if (a > 0 && b > 0 && result < 0) return true;
        if (a < 0 && b < 0 && result > 0) return true;
        return false;
    }

    void reduce() {
        if (numerator_ == 0) {
            denominator_ = 1;
            return;
        }
        
        int64_t gcd_val = std::gcd(std::abs(numerator_), denominator_);
        numerator_ /= gcd_val;
        denominator_ /= gcd_val;
    }

    static Rational add_safe(const Rational& a, const Rational& b) {
        uint64_t gcd_denominators = std::gcd(a.denominator_, b.denominator_);
        uint64_t simp_den1 = a.denominator_ / gcd_denominators;
        uint64_t simp_den2 = b.denominator_ / gcd_denominators;
        
        int64_t term1, term2;
        
        if (mul_overflow(a.numerator_, simp_den2)) {
            return Rational(a.numerator_ * b.denominator_ + b.numerator_ * a.denominator_, 
                          a.denominator_ * b.denominator_);
        }
        term1 = a.numerator_ * simp_den2;
        
        if (mul_overflow(b.numerator_, simp_den1)) {
            return Rational(a.numerator_ * b.denominator_ + b.numerator_ * a.denominator_, 
                          a.denominator_ * b.denominator_);
        }
        term2 = b.numerator_ * simp_den1;
        
        if (add_overflow(term1, term2)) {
            return Rational(a.numerator_ * b.denominator_ + b.numerator_ * a.denominator_, 
                          a.denominator_ * b.denominator_);
        }
        
        int64_t new_numerator = term1 + term2;
        uint64_t new_denominator = a.denominator_ * simp_den2;
        
        return Rational(new_numerator, new_denominator);
    }

public:
    Rational() : numerator_(0), denominator_(1) {}
    
    Rational(int64_t n) : numerator_(n), denominator_(1) {}
    
    Rational(int64_t num, int64_t denom) {
        if (denom == 0) {
            denominator_ = 1;
            numerator_ = 0;
        } else if (denom < 0) {
            numerator_ = -num;
            denominator_ = -denom;
        } else {
            numerator_ = num;
            denominator_ = denom;
        }
        reduce();
    }
    
    int64_t numerator() const { return numerator_; }
    uint64_t denominator() const { return denominator_; }
    
    explicit operator double() const {
        return static_cast<double>(numerator_) / static_cast<double>(denominator_);
    }
    
    std::string str() const {
        if (denominator_ == 1) {
            return std::to_string(numerator_);
        } else {
            return std::to_string(numerator_) + "/" + std::to_string(denominator_);
        }
    }
    
    Rational operator-() const {
        return Rational(-numerator_, denominator_);
    }

    Rational operator+(const Rational& other) const {
        if (mul_overflow(numerator_, other.denominator_) ||
            mul_overflow(other.numerator_, denominator_) ||
            mul_overflow(static_cast<int64_t>(denominator_), 
                                 static_cast<int64_t>(other.denominator_))) {
            return add_safe(*this, other);
        }
        
        int64_t new_numerator = numerator_ * other.denominator_ + other.numerator_ * denominator_;
        uint64_t new_denominator = denominator_ * other.denominator_;
        return Rational(new_numerator, new_denominator);
    }
    
    Rational& operator+=(const Rational& other) {
        *this = *this + other;
        return *this;
    }

    Rational operator-(const Rational& other) const {
        return *this + (-other);
    }
    
    Rational& operator-=(const Rational& other) {
        *this = *this - other;
        return *this;
    }

    Rational operator*(const Rational& other) const {
        if (mul_overflow(numerator_, other.numerator_) ||
            mul_overflow(static_cast<int64_t>(denominator_), 
                                 static_cast<int64_t>(other.denominator_))) {
            Rational a = *this;
            Rational b = other;
            uint64_t gcd1 = std::gcd(std::abs(a.numerator_), b.denominator_);
            uint64_t gcd2 = std::gcd(std::abs(b.numerator_), a.denominator_);
            
            a.numerator_ /= gcd1;
            b.denominator_ /= gcd1;
            b.numerator_ /= gcd2;
            a.denominator_ /= gcd2;
            
            return Rational(a.numerator_ * b.numerator_, a.denominator_ * b.denominator_);
        }
        
        int64_t new_numerator = numerator_ * other.numerator_;
        uint64_t new_denominator = denominator_ * other.denominator_;
        return Rational(new_numerator, new_denominator);
    }
    
    Rational& operator*=(const Rational& other) {
        *this = *this * other;
        return *this;
    }

    Rational operator/(const Rational& other) const {
        if (other.numerator_ == 0) {
            return Rational(0); 
        }
        return *this * Rational(other.denominator_, other.numerator_);
    }
    
    Rational& operator/=(const Rational& other) {
        *this = *this / other;
        return *this;
    }

    bool operator==(const Rational& other) const {
        return numerator_ == other.numerator_ && denominator_ == other.denominator_;
    }
    
    bool operator!=(const Rational& other) const {
        return !(*this == other);
    }
    
    bool operator<(const Rational& other) const {
        return numerator_ * other.denominator_ < other.numerator_ * denominator_;
    }
    
    bool operator<=(const Rational& other) const {
        return *this < other || *this == other;
    }
    
    bool operator>(const Rational& other) const {
        return !(*this <= other);
    }
    
    bool operator>=(const Rational& other) const {
        return !(*this < other);
    }
};


int main() {
    int64_t num1, den1;
    std::cout << "Числитель первой дроби: ";
    std::cin >> num1;
    std::cout << "Знаменатель первой дроби: ";
    std::cin >> den1;

    int64_t num2, den2;
    std::cout << "Числитель второй дроби: ";
    std::cin >> num2;
    std::cout << "Знаменатель второй дроби: ";
    std::cin >> den2;

    Rational r1(num1, den1);
    Rational r2(num2, den2);
    
    std::cout << "\nВид дробей:" << std::endl;
    std::cout << "Дробь 1: " << r1.str() << std::endl;
    std::cout << "Дробь 2: " << r2.str() << std::endl;
    
    std::cout << "\nАрифметические операции:" << std::endl;
    
    Rational sum = r1 + r2;
    std::cout << r1.str() << " + " << r2.str() << " = " << sum.str() << " = " << static_cast<double>(sum) << std::endl;

    Rational diff = r1 - r2;
    std::cout << r1.str() << " - " << r2.str() << " = " << diff.str() << " = " << static_cast<double>(diff) << std::endl;

    Rational product = r1 * r2;
    std::cout << r1.str() << " * " << r2.str() << " = " << product.str() << " = " << static_cast<double>(product) << std::endl;

    if (r2.numerator() != 0) {
        Rational quotient = r1 / r2;
        std::cout << r1.str() << " / " << r2.str() << " = " << quotient.str() << " = " << static_cast<double>(quotient) << std::endl;
    } else {
        std::cout << r1.str() << " / " << r2.str() << " = Ошибка: деление на ноль!" << std::endl;
    }
    
    std::cout << "\nОперации сравнения" << std::endl;
    std::cout << std::boolalpha;
    std::cout << r1.str() << " == " << r2.str() << " : " << (r1 == r2) << std::endl;
    std::cout << r1.str() << " != " << r2.str() << " : " << (r1 != r2) << std::endl;
    std::cout << r1.str() << " <  " << r2.str() << " : " << (r1 < r2) << std::endl;
    std::cout << r1.str() << " >  " << r2.str() << " : " << (r1 > r2) << std::endl;
    std::cout << r1.str() << " <= " << r2.str() << " : " << (r1 <= r2) << std::endl;
    std::cout << r1.str() << " >= " << r2.str() << " : " << (r1 >= r2) << std::endl;
    
    return 0;
}