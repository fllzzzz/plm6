import { supplierIsHideEnum } from '@enum-ms/supplier'

// 获取供应商列表
const getSupplierList = {
  url: '/api/wms/supplier',
  method: 'get',
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        'content|3': [{
          'id|+1': 1,
          'name|+1': ['山东万马物流有限公司', '杭州物产贸易有限公司', '临时零星货运物流供应商'],
          'area|+1': ['山东省济南市', '浙江省杭州市江干区九环路9号', '浙江省宁波市海曙区阿达阿达'],
          'supplierClassification|2-3': 3,
          'enabled': supplierIsHideEnum.FALSE.V,
          'createTime': '@datetime(T)'
        }],
        'totalElements': 3
      }
    }
  }
}

// 获取供应商详情
const getSupplierDetail = {
  url: RegExp('/api/wms/supplier/' + '\\d'),
  method: 'get',
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        'address': null,
        'attachments': [{
          'id': 3,
          'name': '-7Q5-gty6XeZ3uT3cS1hc-u0.jpg',
          'imageUrl': 'http://wfxhxg.natappfree.cc/files\\supplier\\1637723900129_-7Q5-gty6XeZ3uT3cS1hc-u0.jpg',
          'tinyImageUrl': 'http://wfxhxg.natappfree.cc/files\\supplier\\1637723900129_-7Q5-gty6XeZ3uT3cS1hc-u0_256.jpg',
          'createUserId': 1,
          'createUserName': 'xxx',
          'type': 50,
          'createTime': 1637723901000
        }],
        'businessTerm': null,
        'cityId': null,
        'cityName': null,
        'companyEmail': null,
        'companyPhone': null,
        'countryId': null,
        'countryName': null,
        'createTime': 1624931744500,
        'enterpriseType': null,
        'bankAccount': null,
        'bankName': null,
        'id': 1,
        'contacts': [{
          name: 1,
          phone: 2,
          email: 3
        }],
        'legalRepresentative': null,
        'mainBusiness': null,
        'name': '河北运输公司',
        'provinceId': null,
        'provinceName': null,
        'regionId': null,
        'regionName': null,
        'registeredCapital': null,
        'registrationDate': null,
        'shortName': null,
        'socialCode': null,
        'supplierClassification': 64,
        'supplierCode': 'HD-2021629-02',
        'supplierNumber': 1,
        'updateTime': null,
        'website': null
      }
    }
  }
}

// 新增供应商
const addSupplier = {
  url: '/api/wms/supplier',
  method: 'post',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 编辑供应商
const editSupplier = {
  url: '/api/wms/supplier',
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 批量新增供应商
const batchAddSupplier = {
  url: '/api/wms/supplier/batch',
  method: 'post',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 修改隐藏状态
const editStatus = {
  url: RegExp('/api/wms/supplier/enabled'),
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 删除供应商
const delSupplier = {
  url: '/api/wms/supplier',
  method: 'delete',
  timeout: 500,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

export default [
  getSupplierList,
  getSupplierDetail,
  editSupplier,
  addSupplier,
  batchAddSupplier,
  editStatus,
  delSupplier
]
