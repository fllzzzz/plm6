<template>
  <div class="app-container">
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :stripe="false"
      :data="crud.data"
      :data-format="dataFormat"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      :span-method="spanMethod"
      style="width: 100%"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column prop="productionLineType" align="center" :show-overflow-tooltip="true" label="生产线" width="120"> </el-table-column>
      <el-table-column prop="classificationName" align="center" :show-overflow-tooltip="true" label="构件类型">
        <template #default="{ row: { sourceRow: row } }">
          <span v-if="row.productionLineType & artifactProductLineEnum.INTELLECT.V">{{ intellectParentType.VL[row.parentType] }}</span>
          <span v-else>{{ row.classificationName }}</span>
        </template>
      </el-table-column>
      <el-table-column align="center" :show-overflow-tooltip="true" label="子分类">
        <template #default="{ row: { sourceRow: row } }">
          <template v-if="row.productionLineType & artifactProductLineEnum.INTELLECT.V">
            <span>{{ row.classificationName }}</span>
            <template v-if="row.parentType & intellectParentType.BRIDGE.V">
              <span v-if="row.minLength && row.maxLength">
                （{{ row.minLength }}mm {{ row.boolContainsMin ? '≤' : '&lt;' }} 长度 {{ row.boolContainsMax ? '≤' : '&lt;' }}
                {{ row.maxLength }}mm）
              </span>
              <span v-else-if="row.minLength">（{{ row.boolContainsMin ? '≥' : '&gt;' }}{{ row.minLength }}mm）</span>
              <span v-else-if="row.maxLength">（{{ row.boolContainsMax ? '≤' : '&lt;' }}{{ row.maxLength }}mm）</span>
            </template>
          </template>
          <span v-else>-</span>
        </template>
      </el-table-column>
      <el-table-column prop="specPrefixSequence" align="center" :show-overflow-tooltip="true" label="构件前缀"> </el-table-column>
      <el-table-column prop="definitionWord" align="center" :show-overflow-tooltip="true" label="标识"> </el-table-column>
      <el-table-column prop="processSequence" :show-overflow-tooltip="true" label="工序" min-width="200"> </el-table-column>
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
import crudApi, { getArtifact } from '@/api/mes/production-config/product-process'
import { ref } from 'vue'
import { configProductProcessArtifactPM as permission } from '@/page-permission/config'

import { artifactProductLineEnum, intellectParentType } from '@enum-ms/mes'
import { isNotBlank, deepClone } from '@data-type/index'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import udOperation from '@crud/UD.operation'
import mForm from './module/form'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const dataFormat = ref([['productionLineType', ['parse-enum', artifactProductLineEnum]]])

const tableRef = ref()
const { crud, CRUD } = useCRUD(
  {
    title: '构件工序',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi, get: getArtifact }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true, extraBox: '' })

// 合并单元格
function spanMethod({ row, column, rowIndex, columnIndex }) {
  if (column.property === 'productionLineType') {
    return {
      rowspan: row.rowspan || 0,
      colspan: 1
    }
  }
  if (column.property === 'classificationName') {
    return {
      rowspan: row.classRowSpan || 0,
      colspan: 1
    }
  }
}

CRUD.HOOK.handleRefresh = (crud, { data }) => {
  let _tableData = []
  const _dataObj = {
    [artifactProductLineEnum.TRADITION.V]: [],
    [artifactProductLineEnum.INTELLECT.V]: []
  }
  if (isNotBlank(data)) {
    const _content = deepClone(data.content)
    for (let i = 0; i < _content.length; i++) {
      const { structureClassificationList: _cList, parentType = null } = _content[i]
      for (let o = 0; o < _cList.length; o++) {
        const _v = _cList[o]
        _v.specPrefixSequence = _v.specPrefixList?.map((v) => `【${v.specPrefix}】`).join('') || ''
        _v.processSequence = _v.productProcessLinkList?.map((v) => `【${v.name}】`).join('→')
        _v.processSequenceIds = _v.productProcessLinkList?.map((v) => v.processId)
        if (parentType) {
          _v.classRowSpan = o === 0 ? _cList.length : 0
        } else {
          _v.classRowSpan = 1
        }
        _dataObj[_v.productionLineType].push({ ..._v })
      }
    }
  }
  for (const item in _dataObj) {
    if (isNotBlank(_dataObj[item]) && _dataObj[item].length) {
      _dataObj[item][0].rowspan = _dataObj[item].length
    }
    _tableData = _tableData.concat(_dataObj[item])
  }
  data.content = _tableData
}
</script>