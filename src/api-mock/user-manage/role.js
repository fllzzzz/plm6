
const getRole = {
  url: '/api/role',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'data': {
        'content': [{
          'id': 1,
          'menus': [15, 16, 213, 214, 215, 216, 17, 217, 218, 220, 221, 18, 222, 223, 224, 225, 19, 226, 227, 20, 21, 228, 229, 230, 231, 232, 233, 22, 234, 235, 236, 237, 238, 239, 23, 240, 241, 242, 243, 244, 245, 24, 246, 247, 248, 249, 250, 25, 251, 252, 253, 26, 254, 32, 262, 33, 34, 264, 265, 266, 267, 268, 269, 270, 271, 272, 273, 35, 274, 275, 276, 277, 278, 279, 280, 36, 281, 282, 283, 284, 285, 286, 287, 37, 288, 289, 290, 40, 41, 293, 294, 295, 296, 42, 301, 302, 303, 304, 43, 297, 298, 299, 300, 44, 305, 306, 307, 45, 308, 309, 310, 46, 47, 311, 312, 313, 48, 314, 315, 316, 49, 317, 318, 319, 50, 51, 320, 56, 57, 334, 58, 335, 59, 336, 60, 337, 61, 338, 62, 339, 63, 340, 341, 342, 75, 76, 351, 77, 352, 95, 383, 384, 96, 385, 386, 387, 388, 3, 99, 101, 102, 103, 104, 105, 152, 153, 154, 155, 106, 107, 139, 140, 141, 142, 108, 109, 143, 144, 145, 146, 110, 111, 147, 148, 149, 150, 151, 168, 170, 171, 137, 172, 173, 174, 175, 100, 408, 414],
          'name': '超级管理员',
          'permission': 'admin',
          'remark': null
        }, {
          'id': 3,
          'menus': [2, 8, 9, 180, 15, 16, 213, 214, 215, 216, 17, 217, 218, 219, 220, 221, 18, 222, 223, 224, 225, 402, 19, 226, 227, 20, 21, 228, 229, 230, 231, 233, 22, 234, 235, 236, 237, 239, 23, 240, 241, 242, 243, 245, 24, 246, 247, 248, 249, 250, 25, 251, 252, 253, 26, 254, 27, 28, 255, 256, 257, 29, 258, 30, 259, 31, 32, 260, 261, 262, 263, 33, 34, 264, 265, 266, 267, 268, 269, 270, 271, 272, 273, 35, 274, 275, 276, 277, 278, 279, 280, 36, 281, 282, 283, 284, 285, 286, 287, 37, 288, 289, 290, 38, 39, 291, 292, 40, 41, 293, 294, 295, 296, 42, 301, 302, 303, 304, 43, 297, 298, 299, 300, 44, 305, 306, 307, 45, 308, 309, 310, 46, 47, 311, 312, 313, 48, 314, 315, 316, 49, 317, 318, 319, 50, 51, 320, 52, 53, 321, 322, 323, 324, 325, 54, 326, 327, 328, 329, 55, 330, 331, 332, 333, 56, 57, 334, 58, 335, 59, 336, 60, 337, 61, 338, 62, 339, 63, 340, 341, 342, 64, 65, 343, 66, 344, 67, 68, 345, 69, 346, 70, 347, 71, 72, 348, 73, 349, 74, 350, 75, 76, 351, 77, 352, 78, 79, 353, 354, 80, 355, 356, 81, 357, 358, 82, 83, 359, 360, 84, 361, 362, 85, 363, 364, 86, 87, 365, 366, 88, 367, 368, 89, 369, 370, 90, 91, 371, 92, 372, 373, 374, 375, 93, 376, 377, 378, 94, 379, 380, 381, 382, 95, 383, 384, 96, 385, 386, 387, 388, 586, 587, 592, 593, 594, 595, 596, 597, 598, 684, 685, 686, 769, 687, 688, 689, 813, 814, 815, 816, 817, 690, 818, 819, 820, 691, 821, 822, 692, 693, 770, 771, 772, 773, 774, 1024, 694, 795, 796, 797, 798, 800, 695, 823, 824, 825, 826, 828, 696, 834, 835, 836, 697, 829, 830, 831, 832, 833, 698, 840, 699, 703, 705, 710, 712, 718, 722, 724, 728, 736, 738, 741, 744, 746, 749, 752, 755, 762, 3, 99, 100, 414, 415, 416, 417, 418, 419, 420, 421, 101, 102, 422, 423, 424, 103, 425, 426, 427, 408, 428, 429, 409, 410, 430, 431, 432, 459, 411, 434, 435, 436, 412, 437, 438, 439, 440, 441, 442, 1005, 1006, 1007, 413, 443, 444, 445, 446, 447, 448, 449, 450, 451, 452, 453, 104, 105, 152, 106, 107, 139, 108, 109, 143, 398, 399, 400, 403, 404, 405, 401, 406, 475, 476, 487, 526, 484, 485, 486, 527, 477, 488, 564, 489, 528, 490, 529, 478, 491, 530, 531, 492, 532, 533, 493, 534, 535, 563, 565, 566, 479, 494, 536, 537, 538, 539, 567, 495, 540, 541, 542, 496, 543, 480, 497, 568, 569, 570, 498, 571, 572, 573, 574, 575, 576, 577, 578, 499, 579, 580, 481, 500, 553, 554, 501, 555, 556, 613, 502, 503, 562, 584, 585, 483, 505, 544, 545, 546, 547, 581, 602, 600, 601, 603, 604, 605, 606, 607, 608, 609, 610, 654, 655, 548, 549, 550, 551, 552, 656, 657, 658, 659, 660, 673, 674, 675, 676, 661, 677, 678, 679, 680, 681, 682, 683, 137, 174, 973, 138, 161, 162, 164, 165, 163, 166, 167, 168, 460, 169, 170, 171, 172, 173, 175, 176, 974, 977, 978, 979, 980, 981, 982, 983, 984, 985, 986, 987, 988, 991, 990, 992, 989, 177, 975, 185, 186, 187, 188, 189, 190, 191, 192, 976, 993, 994, 995, 996, 997, 998, 999, 1000],
          'name': '高管',
          'permission': '1',
          'remark': ''
        }, {
          'id': 6,
          'menus': [3, 99, 100, 414, 415, 416, 417, 418, 419, 420, 421, 101, 102, 422, 423, 424, 103, 425, 426, 427, 408, 428, 429, 409, 410, 430, 431, 432, 411, 434, 435, 436, 412, 437, 438, 439, 440, 441, 442, 413, 443, 444, 445, 446, 447, 448, 449, 450, 451, 452, 453, 616, 617, 628, 629, 638, 630, 632, 1123, 619, 639, 640, 1069, 620, 621, 641, 642, 643, 645, 1120, 623, 652, 653, 1072, 1350, 1351, 1],
          'name': '合同经理',
          'permission': null,
          'remark': ''
        }, {
          'id': 8,
          'menus': [],
          'name': '计划经理',
          'permission': '6',
          'remark': null
        }],
        'endRow': 44,
        'firstPage': true,
        'hasNextPage': false,
        'hasPreviousPage': false,
        'lastPage': true,
        'navigateFirstPage': 1,
        'navigateLastPage': 1,
        'navigatePageNums': [1],
        'navigatePages': 8,
        'nextPage': 0,
        'pageNum': 1,
        'pageSize': 50,
        'pages': 1,
        'prePage': 0,
        'size': 44,
        'startRow': 1,
        'totalElements': 44
      },
      'message': '操作成功'
    }
  }
}

const addRole = {
  url: '/api/role',
  method: 'post',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '操作成功',
      'data': 10
    }
  }
}

const editRole = {
  url: '/api/role',
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '操作成功',
      'data': 10
    }
  }
}

const delRole = {
  url: '/api/role',
  method: 'delete',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功'
    }
  }
}
const bindRoleMenu = {
  url: '/api/role/menu',
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '操作成功'
    }
  }
}

const roleAll = {
  url: '/api/role/all',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '操作成功',
      'data': {
        'totalElements': 4,
        'content': [{
          'createTime': 1631790427000,
          'id': 1,
          'is_default': true,
          'name': '超级管理员',
          'permission': 'admin',
          'remark': null
        }, {
          'createTime': 1632208283256,
          'id': 2,
          'is_default': true,
          'name': 'hfgh',
          'permission': 'admin',
          'remark': null
        }, {
          'createTime': 1632470386572,
          'id': 3,
          'is_default': false,
          'name': '测试',
          'permission': null,
          'remark': null
        }, {
          'createTime': 1637138894399,
          'id': 4,
          'is_default': false,
          'name': '1',
          'permission': null,
          'remark': '1'
        }]
      }
    }
  }
}

export default [
  getRole,
  addRole,
  editRole,
  delRole,
  bindRoleMenu,
  roleAll
]
