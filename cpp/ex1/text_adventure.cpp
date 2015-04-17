/**
 * @file    text_adventure.cpp
 * @author  Tsvetan Dimitrov <tsvetan.dimitrov23@gmail.com>
 * @date    2015-04-17
 *
 * @brief   Interactive shell program.
 * @details
 */
#include <iostream>
#include <map>
#include <functional>

using namespace std;

typedef map<string, function<void(void)>> command_dict;

float getWeight()
{
    float weight;
    cout << "How much do you weigh (kg)?" << endl;
    cin >> weight;
    return weight;
}

float getHeight()
{
    float height;
    cout << "How tall are you (cm)?" << endl;
    cin >> height;
    return height;
}

/**
 * Calculate Body Mass Index
 */
void bmi()
{
    float weight = getWeight();
    float height = getHeight();
    height /=100;
    float bmi = (weight) / (height * height);
    cout << "Your Body Mass Index (BMI) is -> " << bmi << endl;
    if (bmi < 18.5)
    {
        cout << "You are underweight!" << endl;
        cout << "You should put some meat on those bones \u0001" << endl;
    }
    else if (bmi > 18.5 && bmi < 24.9)
    {
        cout << "Your weight is normal!" << endl;
        cout << "You are probably really hot \u0001" << endl;
    }
    else if (bmi > 25 && bmi < 29.9)
    {
        cout << "You are a bit overweight!" << endl;
        cout << "Go run in the park \u0001" << endl;
    }
    else if (bmi > 30)
    {
        cout << "You are one fat motherfucker!" << endl;
        cout << "Obesity is a very dangerous disease. Please take measures \u0001" << endl;
    }

}

/**
 * Calculate Basal Metabolic Rate
 */
void bmr()
{
    float weight = getWeight();
    float height = getHeight();
    int age;
    cout << "How old are you?" << endl;
    cin >> age;

    char gender;
    cout << "Male of Female [m/f]?" << endl;
    cin >> gender;
    float bmr;
    while(true)
    {
        if (gender == 'm')
        {
            bmr = (float) (66 + (13.7 * weight) + (5 * height) - (6.8 * age));
            break;
        }
        else if (gender =='f')
        {
            bmr = (float) (655 + (9.6 * weight) + (1.8 * height) - (4.7 * age));
            break;
        }
        else
        {
            cout << "Invalid input! Press \'m\' or \'f\'..." << endl;
            continue;
        }
    }

    cout << "Your Basal Metabolic Rate (BMR) is -> " << bmr << endl;
}

/**
 * Displays help menu
 */
void help()
{
    cout << ">>>>>>>>>>> Welcome to your new fit text adventure <<<<<<<<<<<<" << endl;
    cout << "Please type one of the following commands: " << endl;
    cout << "* bmi - calculates your Body Mass Index" << endl;
    cout << "* bmr - calculates your Basal Metabolic Rate" << endl;
    cout << "* help - display this usage guide" << endl;
    cout << "* quit - exit from the program" << endl;
}

/**
 * Exits the program
 */
void quit()
{
    cout << "Bye, bye!!!" << endl;
    exit(0);
}

int main()
{
    command_dict c;
    c["bmi"] = &bmi;
    c["bmr"] = &bmr;
    c["help"] = &help;
    c["quit"] = &quit;

    string input;
    while(true)
    {
        cout << ">>";
        cin >> input;

        //Returns an iterator to the matching element if it is found
        auto it = c.find(input);

        //If the iterator is not pointing to one past the last element
        if(it != end(c))
        {
            //Execute the command
            (it->second)();
        }
        else
        {
            cout << "Command \"" << input << "\" not known" << endl;
        }
    }
}
