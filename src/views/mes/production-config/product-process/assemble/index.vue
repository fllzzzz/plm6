<template>
  <div class="app-container">
    <!--工具栏-->
    <div class="head-container">
      <mHeader />
    </div>
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :stripe="false"
      :data="crud.data"
      :data-format="dataFormat"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      style="width: 100%"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <!-- <el-table-column prop="productionLineTypeEnum" align="center" :show-overflow-tooltip="true" label="生产线" width="120"> </el-table-column> -->
      <el-table-column prop="name" align="center" :show-overflow-tooltip="true" label="部件类型">
        <template #default="{ row }">
          <span>{{ row.name }}</span>
          <template v-if="row.parentType & intellectParentType.BRIDGE.V">
            <span v-if="row.minLength && row.maxLength">
              （{{ row.minLength }}mm {{ row.boolContainsMin ? '≤' : '&lt;' }} 长度 {{ row.boolContainsMax ? '≤' : '&lt;' }}
              {{ row.maxLength }}mm）
            </span>
            <span v-else-if="row.minLength">（{{ row.boolContainsMin ? '≥' : '&gt;' }}{{ row.minLength }}mm）</span>
            <span v-else-if="row.maxLength">（{{ row.boolContainsMax ? '≤' : '&lt;' }}{{ row.maxLength }}mm）</span>
          </template>
          <el-tag v-if="row?.parentType" style="margin-left: 5px">{{ intellectParentType.VL[row.parentType] }}</el-tag>
        </template>
      </el-table-column>
      <el-table-column prop="specPrefixSequence" align="center" :show-overflow-tooltip="true" label="部件前缀"> </el-table-column>
      <el-table-column prop="productProcessLinkList" label="工序" min-width="200">
        <template #default="{ row: { sourceRow: row } }">
          <el-tooltip :content="`${row.processSequence}`" placement="top-start">
            <div style="display: flex; align-items: center; flex-wrap: wrap; white-space: nowrap">
              <div style="display: flex; align-items: center" v-for="(item, index) in row.productProcessLinkList" :key="item.id">
                <span>【{{ item.name }}】</span>
                <div
                  style="
                    display: flex;
                    align-items: center;
                    flex-direction: column;
                    height: 35px;
                    width: 25px;
                    text-align: center;
                    justify-content: center;
                    position: relative;
                  "
                  v-if="index !== row.productProcessLinkList.length - 1"
                >
                  <div
                    v-if="item.nodeTime"
                    style="position: absolute; top: -5px; font-weight: 600; left: 44%; transform: translateX(-50%)"
                    class="tc-warning"
                  >
                    {{ item.nodeTime }}
                  </div>
                  <div style="font-size: 25px; margin-top: 1px; position: absolute; top: 50%; transform: translateY(-50%)">→</div>
                </div>
              </div>
            </div>
          </el-tooltip>
        </template>
      </el-table-column>
      <!--编辑与删除-->
      <el-table-column v-permission="[...permission.edit]" label="操作" width="100px" align="center" fixed="right">
        <template v-slot="scope">
          <udOperation :data="scope.row" :showDel="false" />
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <mForm />
  </div>
</template>

<script setup>
import crudApi, { getAssemble } from '@/api/mes/production-config/product-process'
import { ref } from 'vue'
import { configProductProcessAssemblePM as permission } from '@/page-permission/config'

import { artifactProductLineEnum, intellectParentType } from '@enum-ms/mes'
// import { isNotBlank, deepClone } from '@data-type/index'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import udOperation from '@crud/UD.operation'
import mForm from './module/form'
import mHeader from './module/header'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const dataFormat = ref([['productionLineTypeEnum', ['parse-enum', artifactProductLineEnum]]])

const tableRef = ref()
const { crud, CRUD } = useCRUD(
  {
    title: '部件工序',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi, get: getAssemble }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

// // 合并单元格
// function spanMethod({ row, column, rowIndex, columnIndex }) {
//   if (column.property === 'productionLineTypeEnum') {
//     return {
//       rowspan: row.rowspan || 0,
//       colspan: 1
//     }
//   }
// }

CRUD.HOOK.handleRefresh = (crud, { data }) => {
  // let _tableData = []
  // const _dataObj = {
  //   [artifactProductLineEnum.TRADITION.V]: [],
  //   [artifactProductLineEnum.INTELLECT.V]: []
  // }
  // if (isNotBlank(data)) {
  //   const _content = deepClone(data.content)
  //   for (let i = 0; i < _content.length; i++) {
  //     const _v = _content[i]
  //     _v.specPrefixSequence = _v.assembleSpecList?.map((v) => `【${v.specPrefix}】`).join('') || ''
  //     _v.processSequence = _v.productProcessLinkList?.map((v) => `【${v.name}】`).join('→')
  //     _v.processSequenceIds = _v.productProcessLinkList?.map((v) => v.processId)
  //     _dataObj[_v.productionLineTypeEnum].push({ ..._v })
  //   }
  // }
  // for (const item in _dataObj) {
  //   if (isNotBlank(_dataObj[item]) && _dataObj[item].length) {
  //     _dataObj[item][0].rowspan = _dataObj[item].length
  //   }
  //   _tableData = _tableData.concat(_dataObj[item])
  // }
  data.content = data.content.map((o) => {
    o.processSequenceObj = {}
    o.specPrefixSequence = o.assembleSpecList?.map((v) => `【${v.specPrefix}】`).join('') || ''
    o.processSequence = o.productProcessLinkList?.map((v) => `【${v.name}】${v.nodeTime ? '→ ' + v.nodeTime + ' ' : ''}`).join('→')
    o.processSequenceIds = o.productProcessLinkList?.map((v) => {
      o.processSequenceObj[v.processId] = v.nodeTime
      return v.processId
    })
    return o
  })
}
</script>
