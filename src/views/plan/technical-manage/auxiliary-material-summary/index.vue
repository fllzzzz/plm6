<template>
  <div class="app-container">
    <template v-if="globalProject">
       <!--工具栏-->
      <div class="head-container">
        <mHeader :project-id="globalProjectId" />
      </div>
      <!--表格渲染-->
      <common-table
        ref="tableRef"
        v-loading="crud.loading"
        :data="crud.data"
        :empty-text="crud.emptyText"
        :max-height="maxHeight"
        :data-format="dataFormat"
        style="width: 100%"
      >
        <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
        <el-table-column
          v-if="columns.visible('name')"
          key="name"
          prop="name"
          :show-overflow-tooltip="true"
          label="名称"
          align="center"
        />
        <el-table-column
          v-if="columns.visible('length')"
          key="length"
          prop="length"
          :show-overflow-tooltip="true"
          label="长度(mm)"
          align="center"
        />
        <el-table-column
          v-if="columns.visible('material')"
          key="material"
          prop="material"
          :show-overflow-tooltip="true"
          label="材质"
          align="center"
        />
        <el-table-column
          v-if="columns.visible('specification')"
          key="specification"
          prop="specification"
          :show-overflow-tooltip="true"
          align="center"
          label="规格"
        />
        <el-table-column
          v-if="columns.visible('quantity')"
          key="quantity"
          prop="quantity"
          label="数量"
          align="center"
        />
        <el-table-column
          v-if="columns.visible('remark')"
          key="remark"
          prop="remark"
          :show-overflow-tooltip="true"
          align="center"
          label="备注"
        />
      </common-table>
      <!--分页组件-->
      <pagination />
    </template>
  </div>
</template>

<script setup>
import crudApi from '@/api/plan/technical-manage/auxiliary-material-summary'
import { ref, watch } from 'vue'

import { DP } from '@/settings/config'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import { mapGetters } from '@/store/lib'
import { auxiliaryMaterialSummaryPM as permission } from '@/page-permission/plan'

import mHeader from './module/header'

const { globalProject, globalProjectId } = mapGetters(['globalProject', 'globalProjectId'])

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud, columns } = useCRUD(
  {
    title: '配套件汇总',
    sort: ['id.asc'],
    permission: { ...permission },
    optShow: { ...optShow },
    requiredQuery: ['projectId'],
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.auxiliary-material-summary',
  paginate: true,
  extraHeight: 40
})

watch(
  () => globalProjectId.value,
  (val) => {
    if (val) {
      crud.query.projectId = globalProjectId.value
      crud.toQuery()
    }
  },
  { immediate: true, deep: true }
)

const dataFormat = ref([
  ['length', ['to-fixed', DP.MES_ARTIFACT_L__MM]]
])
</script>

<style lang="scss" scoped>
::v-deep(.abnormal-row) {
  background: #e8f4ff;
}
::v-deep(.hidden-select) {
  td:nth-child(1) {
    .cell {
      opacity: 0;
    }
  }
}
$font-size: 1.5em;
.child {
  width: $font-size;
  height: $font-size;
  display: inline-block;
  border: 1px solid;
  border-radius: 50%;
  line-height: $font-size;
}
</style>
