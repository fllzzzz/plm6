<template>
  <div>
    <!--表格渲染-->
    <div>
      <el-tag type="success" size="medium" style="margin-right:10px;">{{(currentRow.projectContent)}}</el-tag>
      <common-button
        size="mini"
        icon="el-icon-plus"
        type="primary"
        @click="crud.toAdd"
      />
    </div>
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      style="width: 100%;margin-top:10px;"
      class="collection-table"
      :stripe="false"
      return-source-data
      :showEmptySymbol="false"
    >
      <el-table-column prop="index" label="序号" align="center" width="50" type="index" />
      <el-table-column key="name" prop="name" label="批次" align="center" />
      <el-table-column key="quantityWork" prop="quantityWork" label="工程量" align="center" />
      <el-table-column key="date" prop="date" label="交货日期" align="center">
        <template v-slot="scope">
          <div>{{ scope.row.date? parseTime(scope.row.date,'{y}-{m}-{d}'): '-' }}</div>
        </template>
      </el-table-column>
      <el-table-column key="remark" prop="remark" label="备注" align="center" />
      <!--编辑与删除-->
      <el-table-column
        label="操作"
        width="190px"
        align="center"
      >
        <template v-slot="scope">
          <udOperation :data="scope.row" :permission="permission"/>
        </template>
      </el-table-column>
    </common-table>
    <mForm :currentRow="currentRow" />
  <!--分页组件-->
  <pagination />
  </div>
</template>

<script setup>
import crudApi from '@/api/enclosure/enclosure-plan/area'
import { ref, defineProps, watch } from 'vue'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import { supplierLogisticsPaymentPM } from '@/page-permission/supply-chain'
import { parseTime } from '@/utils/date'

import pagination from '@crud/Pagination'
import udOperation from '@crud/UD.operation'
import mForm from './form'

const permission = supplierLogisticsPaymentPM.application

const optShow = {
  add: true,
  edit: false,
  del: false,
  download: false
}

const props = defineProps({
  currentRow: {
    type: Object,
    default: () => {}
  },
  visibleValue: {
    type: Boolean,
    default: false
  }
})

const tableRef = ref()
const { CRUD, crud } = useCRUD(
  {
    title: '批次计划',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    requiredQuery: ['techId'],
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.plan-form',
  paginate: true,
  extraHeight: 40
})

watch(
  () => props.visibleValue,
  (val) => {
    if (val) {
      crud.query.techId = props.currentRow?.id
      crud.toQuery()
    }
  },
  { deep: true, immediate: true }
)

CRUD.HOOK.handleRefresh = (crud, res) => {
  // res.data.content = res.data.content.map((v) => {
  //   return v
  // })
}
</script>

<style lang="scss" scoped>
::v-deep(.abnormal-row) {
  background: #e8f4ff;
}
::v-deep(.hidden-select) {
  td:nth-child(1){
    .cell{
      opacity:0;
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
.collection-table{
  ::v-deep(.el-select .el-input__inner){
    padding-left:2px;
    padding-right:5px;
  }
  ::v-deep(.el-input-number .el-input__inner, .el-input__inner) {
    text-align: left;
    padding:0 5px;
  }
  ::v-deep(.el-table .cell){
    padding-left:2px;
    padding-right:2px;
  }
}

::v-deep(.pass-tag){
  padding:0 50px;
}
</style>
