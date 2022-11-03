<template>
  <div class="app-container">
    <div class="head-container">
      <mHeader @change-mode="handleModeChange" />
    </div>
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      style="width: 100%"
      @sort-change="handleSortChange"
      @selection-change="crud.selectionChangeHandler"
    >
      <el-table-column type="selection" width="55" align="center" />
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column
        v-if="columns.visible('monomer.name')"
        prop="monomer.name"
        :show-overflow-tooltip="true"
        label="单体"
        min-width="120px"
        align="center"
      />
      <el-table-column
        v-if="columns.visible('area.name')"
        prop="area.name"
        :show-overflow-tooltip="true"
        label="区域"
        min-width="120px"
        align="center"
      />
      <el-table-column
        v-if="columns.visible('serialNumber')"
        prop="serialNumber"
        :show-overflow-tooltip="true"
        label="部件编号"
        min-width="120px"
        align="center"
      >
        <template #default="{ row }">
          <!-- <div v-for="item in row.artifactTypesettingDTOS" :key="item"> -->
            <!-- 暂时写死，目前只有钢柱、长短梁、短梁 -->
            <table-cell-tag
              v-if="row.productionLineTypeEnum === artifactProductLineEnum.INTELLECT.V"
              :name="row.classificationName"
              color="#fad400"
              :offset="15"
            />
            <!-- <table-cell-tag
              v-if="row.productionLineTypeEnum === artifactProductLineEnum.INTELLECT.V && row.classificationName === '长短梁'"
              name="长短梁"
              color="#40ed8d"
              :offset="15"
            /> -->
            <!-- <table-cell-tag
              v-if="row.productionLineTypeEnum === artifactProductLineEnum.INTELLECT.V && row.classificationName === '短梁'"
              name="短梁"
              color="#00babd"
              :offset="15"
            /> -->
            <span>{{ row.serialNumber }}</span>
          <!-- </div> -->
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('quantity')"
        prop="quantity"
        :show-overflow-tooltip="true"
        label="数量"
        align="center"
        width="120px"
      >
        <template #default="{ row: { sourceRow: row } }">
          <el-input-number
            v-if="curEditMode === 'edit'"
            v-model="row.editQuantity"
            :step="1"
            :min="0"
            :max="row.quantity"
            :precision="0"
            size="mini"
            controls-position="right"
            style="width: 100%"
          />
          <span v-else>{{ row.quantity }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('artifactStr')"
        prop="artifactStr"
        :show-overflow-tooltip="true"
        label="关联构件"
        min-width="160px"
      />
      <el-table-column
        v-if="columns.visible('specification')"
        prop="specification"
        :show-overflow-tooltip="true"
        label="规格"
        min-width="100px"
        align="center"
      />
      <el-table-column
        v-if="columns.visible('length')"
        prop="length"
        :show-overflow-tooltip="true"
        label="长度（mm）"
        sortable="custom"
        align="center"
        width="120px"
      />
      <el-table-column
        v-if="columns.visible('material')"
        prop="material"
        :show-overflow-tooltip="true"
        label="材质"
        align="center"
        width="100px"
      />
      <el-table-column
        v-if="columns.visible('netWeight')"
        prop="netWeight"
        :show-overflow-tooltip="true"
        label="单重（kg）"
        align="center"
        width="100px"
      />
      <el-table-column
        v-if="columns.visible('boolHaveNC1')"
        prop="boolHaveNC1"
        :show-overflow-tooltip="true"
        label="NC1"
        width="80px"
        align="center"
      >
        <template #default="{ row }">
          <span v-if="row.boolHaveNC1" class="tc-success">√</span>
          <span v-else>-</span>
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <!-- <pagination /> -->
  </div>
</template>

<script setup>
import crudApi from '@/api/mes/craft-manage/section-steel/nesting-setting'
import { ref } from 'vue'
import { artifactProductLineEnum } from '@enum-ms/mes'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
// import pagination from '@crud/Pagination'
import mHeader from './module/header'

// crud交由presenter持有
const permission = {
  get: [''],
  edit: [''],
  add: [''],
  del: ['']
}

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '套料设置',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: false,
    queryOnPresenterCreated: false,
    requiredQuery: ['projectId']
  },
  tableRef
)

const { maxHeight } = useMaxHeight()

const curEditMode = ref('nesting')

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    v.editQuantity = v.quantity
    v.artifactStr =  v.artifactTypesettingDTOS.map(o=>o.serialNumber)?.join('，') || ''
    v.classificationName = v.artifactTypesettingDTOS.map(o=>o.classificationName)[0]
    return v
  })
}

// 长度排序 true 升序 false 降序
function handleSortChange({ column, prop, order }) {
  if (prop === 'length') {
    crud.query.boolLength = order === 'ascending'
  } else {
    crud.query.boolLength = false
  }
  crud.toQuery()
}

function handleModeChange(mode) {
  curEditMode.value = mode
}
</script>
