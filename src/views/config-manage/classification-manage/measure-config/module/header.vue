<template>
  <div class="head-container">
    <crudOperation>
      <template #optLeft>
        <el-input v-model.trim="query.nameOrCode" placeholder="输入科目名称、编号搜索" class="filter-item" style="width: 250px" size="small" clearable />
      </template>
      <template v-permission="permission.edit" #viewLeft>
        <common-button class="filter-item" type="primary" size="mini" @click="configDetail(1)">一级科目设置</common-button>
        <common-button class="filter-item" type="primary" size="mini" @click="configDetail(2)">二级科目设置</common-button>
        <common-button class="filter-item" type="primary" size="mini" @click="configDetail(3)">三级科目设置</common-button>
      </template>
    </crudOperation>

    <!-- 计量配置 -->
    <config-view
      v-model:visible="config.visible"
      :level="config.level"
      :classificationList="classificationMap.get(config.level)"
    />
  </div>
</template>

<script setup>
import { inject, provide, ref, reactive } from 'vue'
import { isNotBlank } from '@data-type'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import configView from './config.vue'

const permission = inject('permission')

const defaultQuery = { nameOrCode: undefined }
const { CRUD, query } = regHeader(defaultQuery)

// 源数据保留
const sourceMap = new Map([
  ['measureUnit', 'sourceMeasureUnit'],
  ['measurePrecision', 'sourceMeasurePrecision'],
  ['accountingUnit', 'sourceAccountingUnit'],
  ['accountingPrecision', 'sourceAccountingPrecision'],
  ['outboundUnit', 'sourceOutboundUnit']
])
provide('sourceMap', sourceMap)

// 科目数据
const classificationMap = ref(new Map())

// 科目设置
const config = reactive({
  level: 3,
  isEdit: false,
  visible: false
})

CRUD.HOOK.handleRefresh = (crud, res) => {
  // 筛选科目类型为材料类型的科目
  classificationMap.value = new Map()
  res.data.content = classificationTreeFormat(res.data.content)
}

function configDetail(level) {
  config.level = level
  config.visible = true
}

function recordSource(row = {}) {
  sourceMap.forEach((val, key) => {
    row[val] = row[key]
  })
  return row
}

function classificationTreeFormat(tree, deep = 1, extendsData = { fullId: [], fullName: [], fullCode: [], fullSerialNumber: [] }) {
  // 表格展示的数据（所有末级科目）
  const tableData = []

  // 当前层的所有节点
  const currentList = new Array(tree.length)

  // 设置层级Map key:1,2,3 value:node
  let levelMap = classificationMap.value.get(deep)

  // 若为空则新建
  if (!levelMap) {
    levelMap = []
    classificationMap.value.set(deep, levelMap)
  }

  // 遍历科目数
  tree.forEach((node, index) => {
    // 因为再单位设置页面用到了下面的一些参数，而单位设置页面不需要继承的信息，因此直接设置在node中
    node.fullId = [...extendsData.fullId, node.id] // id-全路径
    node.fullName = [...extendsData.fullName, node.name] // name-全路径
    node.fullCode = [...extendsData.fullCode, node.code] // code（编码）-全路径
    node.fullSerialNumber = [...extendsData.fullSerialNumber, extendsData.fullCode.join('') + node.code] // 科目完整编码-全路径
    // 单位设置时用的map
    currentList[index] = recordSource(node)
    // 设置节点信息
    const leafNode = {}
    Object.assign(leafNode, JSON.parse(JSON.stringify(extendsData)), node)

    // 设置当前层信息
    const nodeChildren = leafNode.children
    if (isNotBlank(nodeChildren)) {
      const e = {
        fullId: leafNode.fullId, // id-全路径
        fullName: leafNode.fullName, // 名称-全路径
        fullCode: leafNode.fullCode, // 编码-路径
        fullSerialNumber: leafNode.fullSerialNumber, // 编码-路径
        attribute: leafNode.attribute, // 类型
        measureUnit: leafNode.measureUnit, // 计量
        accountingUnit: leafNode.accountingUnit, // 核算单位
        measurePrecision: leafNode.measurePrecision, // 计量精度
        accountingPrecision: leafNode.accountingPrecision // 核算精度
      }
      const leafNodes = classificationTreeFormat(nodeChildren, deep + 1, e)
      tableData.push.apply(tableData, leafNodes)
    } else {
      tableData.push(leafNode)
    }
  })

  // 将当前层的数据塞入对应的科目等级的数组中
  levelMap.push.apply(levelMap, tree)
  return tableData
}
</script>
