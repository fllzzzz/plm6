// 获取公司信息
const getCompanyConfig = {
  url: '/api/config/company',
  timeout: 500,
  method: 'get',
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        'companyName': '初鸣智建',
        'companyNo': '1',
        'website': null,
        'telephone': null
      }
    }
  }
}

// 设置公司信息
const setCompanyConfig = {
  url: '/api/config/company',
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 获取项目信息
const getProjectConfig = {
  url: '/api/config/project',
  timeout: 500,
  method: 'get',
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        'showProjectFullName': true,
        'showSerialNumber': true,
        'arrangement': 1
      }
    }
  }
}

// 设置项目信息
const setProjectConfig = {
  url: '/api/config/project',
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 获取公司logo列表
const getLogoConfig = {
  url: '/api/config/company/logo',
  timeout: 500,
  method: 'get',
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        'totalElements': 2,
        'content': [
          {
            'id': 67,
            'path': 'https://mes.dev.hzchum.com/files/logo/1606631709892_logo.0bc03a9a.png',
            'isDefault': true
          },
          {
            'id': 69,
            'path': 'https://mes.dev.hzchum.com/files/logo/1606631728140_cmlogo.png',
            'isDefault': false
          }
        ]
      }
    }
  }
}

// 设置公司默认logo
const setLogoConfig = {
  url: RegExp('/api/config/company/logo/default/' + '\\d'),
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 删除公司logo
const delLogoConfig = {
  url: RegExp('/api/config/company/logo/' + '\\d'),
  method: 'delete',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: true
    }
  }
}

export default [getCompanyConfig, setCompanyConfig, getProjectConfig, setProjectConfig, getLogoConfig, setLogoConfig, delLogoConfig]
