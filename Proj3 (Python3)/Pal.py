from typing import List
class Solution:
    def longestCommonPrefix(strs: List[str]) -> str:
        prefix = ""
        i = 0
        areSame = True
        minL = min(map(len,strs))
        while(i < minL):
            temp = strs[0][i]
            for elem in strs:
                if(elem[i] != temp):
                    return prefix
            prefix = prefix + str(temp)
            i = i + 1        
        return prefix
    print(longestCommonPrefix(["yousuck","yougay","youhoe"]))