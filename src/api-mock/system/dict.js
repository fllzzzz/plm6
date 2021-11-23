const getDict = {
  url: '/api/dict',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '操作成功',
      'data': {
        'totalElements': 13,
        'content': [{
          'name': 'margin_type',
          'remark': '合同-保证金类型',
          'dictDetails': [{
            'id': 3,
            'name': 'margin_type',
            'remark': '合同-保证金类型',
            'label': '保函',
            'sort': 1,
            'createTime': 1632476140017
          }, {
            'id': 4,
            'name': 'margin_type',
            'remark': '合同-保证金类型',
            'label': '现金',
            'sort': 2,
            'createTime': 1632478158633
          }]
        }, {
          'name': 'structure_type',
          'remark': '合同-结构类型',
          'dictDetails': null
        }, {
          'name': 'currency_type',
          'remark': '币种类型',
          'dictDetails': [{
            'id': 5,
            'name': 'currency_type',
            'remark': '币种类型',
            'label': '人民币',
            'sort': 1,
            'createTime': 1632478182568
          }, {
            'id': 6,
            'name': 'currency_type',
            'remark': '币种类型',
            'label': '美元',
            'sort': 2,
            'createTime': 1632478217276
          }, {
            'id': 7,
            'name': 'currency_type',
            'remark': '币种类型',
            'label': '欧元',
            'sort': 3,
            'createTime': 1632478225067
          }]
        }, {
          'name': 'reason_type',
          'remark': '变更原因',
          'dictDetails': null
        }, {
          'name': 'unit',
          'remark': '单位',
          'dictDetails': null
        }, {
          'name': 'quality_trouble_type',
          'remark': '问题类型',
          'dictDetails': [{
            'id': 8,
            'name': 'quality_trouble_type',
            'remark': '问题类型',
            'label': '回家看看',
            'sort': 1,
            'createTime': 1632709787575
          }]
        }, {
          'name': 'enterprise_type',
          'remark': '企业类型',
          'dictDetails': null
        }, {
          'name': 'order_type',
          'remark': '订单类型',
          'dictDetails': null
        }, {
          'name': 'payment_reason',
          'remark': '付款事由',
          'dictDetails': null
        }, {
          'name': 'material_supplier_fee_type',
          'remark': '物料供应商费用种类',
          'dictDetails': null
        }, {
          'name': 'reimbursement_type',
          'remark': '项目报销类型',
          'dictDetails': null
        }, {
          'name': 'logistics_supplier_fee_type',
          'remark': '物流供应商费用种类',
          'dictDetails': null
        }, {
          'name': 'mes_qhse_product_problem_type',
          'remark': '生产QHSE产品问题种类',
          'dictDetails': null
        }]
      }
    }
  }
}

export default [
  getDict
]
