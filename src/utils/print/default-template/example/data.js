import Mock from 'mockjs'
const Random = Mock.Random

Random.extend({
  bankName: function () {
    // 银行名称
    var name = ['中国工商银行', '中国建设银行', '中国银行', '招商银行', '中国农业银行', '交通银行', '中信银行']
    return this.pick(name)
  },
  address: function () {
    // 地址
    var name = [
      '杭州市江干区杭海路888号',
      '杭州市江干区蕙兰雅路697号',
      '杭州市余杭区乔司街道乔莫东路61号',
      '湖州市长兴县明珠路511号',
      '湖州市长兴县金陵中路666'
    ]
    return this.pick(name)
  },
  psname: function () {
    // 项目名称
    var name = [
      '马奇诺防线',
      '图什卡工程',
      '瓦伊昂大坝',
      '曼哈顿',
      '螺旋塔',
      '勃兰登堡机场',
      '杭州湾跨海大桥',
      '京津城际轨道交通',
      '武汉长江隧道',
      '襄渝铁路二线',
      '温福铁路',
      '宜万铁路',
      '石太客运专线',
      '辛辛那提（Cincinnati）地铁',
      '西单大悦城',
      '北京国贸1期',
      '北京世纪金源综合体',
      '力宝广场',
      '盘古大观',
      '三里屯SOHO',
      '上海五角广场万达广场',
      '上海新天地',
      '恒隆广场',
      '白金湾滨江综合体',
      '京基100',
      '广州太古汇',
      '武汉新世界k11艺术购物广场',
      '武汉世界城光谷步行街',
      '成都音乐东区公园',
      '成都新世纪环球中心',
      '重庆龙湖时代天街',
      '南亚风情第一城',
      '香港IFC',
      '天津星耀五洲',
      '南京水游城',
      '澳门威尼斯人',
      '沈阳万象城',
      '石家庄新源NASA',
      '呼和佳地成吉思汗中央广场',
      '苏州圆融时代广场',
      '大同东小城',
      '台北101',
      '太古广场 ',
      '佛山岭南天地',
      '苏州广电总台国际影视娱乐城',
      '英吉利海峡隧道',
      '关西国际机场',
      '加利福尼亚高速铁路',
      '迪拜乐园',
      '阿卜杜勒国王经济城',
      '卡沙干油田'
    ]
    return this.pick(name)
  },
  monomerName: function () {
    // 单体名称
    var name = [
      '博物馆',
      '龙奥大厦',
      '五角大楼',
      '新世纪环球中心',
      '体育馆',
      '研究中心',
      '博览中心',
      '会展中心',
      '大兴机场',
      '中心大厦',
      '中国尊',
      '无尽大楼',
      '王国塔',
      'x-seed4000',
      '旋转摩天大楼'
    ]
    return this.pick(name)
  },
  areaName: function () {
    // 区域名称 Mock.mock('@character') + '区'
    var name = [
      'A区',
      'B区',
      'C区',
      'D区',
      'E区',
      'F区',
      'G区',
      'H区',
      'I区',
      'J区',
      'K区',
      'L区',
      'M区',
      'N区',
      'O区',
      'P区',
      'Q区',
      'R区',
      'S区',
      'T区',
      'U区',
      'V区',
      'W区',
      'X区',
      'Y区',
      'Z区'
    ]
    return this.pick(name)
  },
  factoryName: function () {
    // 工厂名称
    var name = ['萧山工厂', '湖滨工厂', '环山工厂']
    return this.pick(name)
  },
  warehouseName: function () {
    // 仓库位置
    var name = ['一号仓', '二号仓', '三号仓']
    return this.pick(name)
  },
  workshop: function () {
    // 生产线名称
    var name = ['一车间', '二车间', '三车间']
    return this.pick(name)
  },
  productionLine: function () {
    // 生产线名称
    var name = ['重钢一线', '重钢二线', '零件班组', '次构件班组', '棱条班组', '围护生产车间', '轻钢一线', '轻钢二线']
    return this.pick(name)
  },
  componentProcess: function () {
    // 工序名称
    var name = ['组立', '总装', '焊接', '清磨', '涂装']
    return this.pick(name)
  },
  structureProcess: function () {
    // 构件工序名称
    var name = ['组立', '打底焊', '埋弧焊', '电渣焊', '总装', '焊接', '抛丸除锈', '清磨', '端铣', '涂装']
    return this.pick(name)
  },
  enclosureProcess: function () {
    // 围护工序名称
    var name = ['压板']
    return this.pick(name)
  },
  machinePartProcess: function () {
    // 零件工序名称
    var name = ['下料', '坡口', '钻孔']
    return this.pick(name)
  },
  segmentsProcess: function () {
    // 箱体工序名称
    var name = ['拼装', '焊接', '校正', '清磨', '喷砂', '涂装']
    return this.pick(name)
  },
  singleElementProcess: function () {
    // 单元件工序名称
    var name = ['装配', '折弯', '装配', '焊接', '校正', '清磨']
    return this.pick(name)
  },
  structureName: function () {
    // 构件名称
    var name = [
      '钢柱',
      '钢梁',
      '箱型柱',
      '劲性箱型柱',
      '方管柱',
      '十字柱',
      '圆管柱',
      '牛腿',
      '吊车梁',
      '女儿墙柱',
      '雨棚梁',
      '锚栓',
      'H型材柱',
      'H型材梁',
      '备带板',
      '次梁',
      '平台花纹板',
      '抗风柱',
      '墙面檩条',
      '门柱',
      '门梁',
      '梯梁',
      '梯柱',
      '网架钢支座',
      '楼梯埋件',
      '直发件',
      '隅撑'
    ]
    return this.pick(name)
  },
  enclosureName: function () {
    // 围护名称
    var name = ['内墙收边件', '外墙收边件', '内墙板', '外墙板', '屋面板']
    return this.pick(name)
  },
  segmentsName: function () {
    // 箱体名称
    var name = ['箱梁', '箱体', '直发件']
    console.log('this.pick: ', this.pick)
    return this.pick(name)
  },
  singleElementName: function () {
    // 单元件名称
    var name = [
      '檐板',
      '端封隔板',
      '端封板',
      '实隔板',
      '挑臂隔板',
      '底板',
      '腹板',
      '面板',
      '挑臂封板',
      'T肋封板',
      '支座隔板',
      '支座封板',
      '装饰板',
      '压重封板',
      '底板人孔盖板'
    ]
    return this.pick(name)
  },
  cpname: function () {
    // 公司名称
    var name = ['杭州初鸣建科', '安徽初鸣建科']
    return this.pick(name)
  },
  dept: function () {
    // 部门
    var dept = ['企划部', '门市部', '业务部', '售后服务部', '检验部', '生产部', '运输部', '采购部', '人力资源部', '风控部', '财务部', '开发部']
    return this.pick(dept)
  },
  reimbursementType: function () {
    // 报销类型
    var reimbursement_type = [
      '差旅费',
      '招待费',
      '礼品费',
      '过路费',
      '交通费',
      '汽车油费',
      '方案设计费',
      '深化设计费',
      '施工方案费',
      '效果图制作费',
      '业务费',
      '海运费',
      '签证费',
      '包装费',
      '施工费',
      '吊装费',
      '安装费',
      '工程费',
      '餐饮费'
    ]
    return this.pick(reimbursement_type)
  },
  supplierFeeType: function () {
    // 供应商费用类型
    var supplier_fee_type = ['货款支付', '货款预付', '物料运输费']
    return this.pick(supplier_fee_type)
  },
  paymentReason: function () {
    // 付款事由
    var text = ['项目预付款', '项目进度款', '项目结算款', '出口退税']
    return this.pick(text)
  },
  orderType: function () {
    // 订单类型
    var order_type = ['加工订单', '工程项目']
    return this.pick(order_type)
  },
  structureType: function () {
    // 结构类型
    var structure_type = ['门式钢架', '框架结构', '网架结构', '桁架结构', '排架结构', '索膜结构', '异性双曲结构', '桥梁']
    return this.pick(structure_type)
  },
  // TODO: 变更，MES5中没有1-3级科目
  typeName: function () {
    // firstClassName 种类名称
    var name = ['钢板']
    return this.pick(name)
  },
  className: function () {
    // secondClassName 类型名称
    var name = ['开平板', '中厚板', '不锈钢板']
    return this.pick(name)
  },
  materialName: function () {
    // thirdClassName 材质名称
    var name = ['Q235B', 'Q355B', 'Q355C', 'Q355D']
    return this.pick(name)
  },
  plateType: function () {
    // 板型
    var type = ['Q-900', 'WA-840', 'RP-464', 'V-600', 'V-860']
    return this.pick(type)
  },
  unit: function () {
    // 单位
    var unit = ['张', '根', '卷', '吨', '千克', '米', '桶', '盘']
    return this.pick(unit)
  },
  measurementUnit: function () {
    // 计量单位
    var unit = ['张', '根', '卷', '吨', '桶', '盘']
    return this.pick(unit)
  },
  accountingUnit: function () {
    // 核算单位
    var unit = ['kg', 't', 'm', 'mm']
    return this.pick(unit)
  },
  color: function () {
    // 颜色
    var color = [
      '黑色',
      '白色',
      '蓝色',
      '黄色',
      '红色',
      '洋红色',
      '紫色',
      '橘红色',
      '粉色',
      '深褐色',
      '粉蓝色',
      '靛蓝色',
      '天蓝色',
      '深蓝色',
      '冰蓝色',
      '浅绿色',
      '草绿色',
      '春绿色',
      '卡其色',
      '浅黄色',
      '淡黄色',
      '红褐色',
      '弱粉色',
      '宝石红',
      '栗色',
      '浅紫色'
    ]
    return this.pick(color)
  },
  brand: function () {
    // 品牌
    var name = ['武钢', '马钢', '莱钢', '日钢']
    return this.pick(name)
  }
})

export default Random
