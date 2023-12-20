<template>
  <div class="app-container">
    <mHeader />
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :data-format="dataFormat"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      style="width: 100%;margin-top:10px;"
      class="collection-table"
      :stripe="false"
    >
      <el-table-column prop="index" label="序号" align="center" width="50" type="index" />
      <el-table-column key="receiptSerialNumber" prop="receiptSerialNumber" v-if="columns.visible('receiptSerialNumber')" label="转换单号" align="center" min-width="150" show-overflow-tooltip />
      <el-table-column key="createTime" v-if="columns.visible('createTime')" prop="createTime" label="转换时间" align="center" show-overflow-tooltip />
      <el-table-column key="outSerialNumber" prop="outSerialNumber" v-if="columns.visible('outSerialNumber')" label="出库单号" align="center" show-overflow-tooltip />
      <el-table-column key="serialNumber" prop="serialNumber" v-if="columns.visible('serialNumber')" label="物料编号" align="center" show-overflow-tooltip width="100"/>
      <el-table-column key="classifyName" prop="classifyName" v-if="columns.visible('classifyName')" label="物料名称" align="center" show-overflow-tooltip width="100" />
      <el-table-column key="specification" prop="specification" v-if="columns.visible('specification')" label="规格" align="center" show-overflow-tooltip/>
      <el-table-column key="widthThick" prop="widthThick" v-if="columns.visible('widthThick')" label="厚(mm)*宽(mm)" align="center" show-overflow-tooltip/>
      <el-table-column key="brand" prop="brand" v-if="columns.visible('brand')" label="品牌" align="center" show-overflow-tooltip/>
      <el-table-column key="color" prop="color" v-if="columns.visible('color')" label="颜色" align="center" show-overflow-tooltip/>
      <el-table-column key="heatNoAndBatchNo" prop="heatNoAndBatchNo" v-if="columns.visible('heatNoAndBatchNo')" label="卷号" align="center" show-overflow-tooltip/>
      <el-table-column key="project" prop="project" v-if="columns.visible('project')" label="项目" align="center" show-overflow-tooltip/>
      <el-table-column key="warehouse.name" prop="warehouse.name" v-if="columns.visible('warehouse.name')" label="仓库" align="center" show-overflow-tooltip/>
      <!--编辑与删除-->
      <el-table-column
        v-if="checkPermission([...permission.detail])"
        label="操作"
        width="190px"
        align="center"
      >
        <template v-slot="scope">
          <common-button icon="el-icon-view" type="primary" size="mini" @click="openDetail(scope.row)" v-permission="permission.detail"/>
        </template>
      </el-table-column>
    </common-table>
    <mDetail :detail-info="currentRow" v-model="detailVisible" />
  <!--分页组件-->
  <pagination />
  </div>
</template>

<script setup>
import crudApi from '@/api/wms/report/raw-material/convert-list'
import { ref, computed } from 'vue'

import checkPermission from '@/utils/system/check-permission'
import { rawMaterialConvertListPM as permission } from '@/page-permission/wms'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import mDetail from './module/detail'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const dataFormat = computed(() => {
  return [
    ['createTime', ['parse-time', '{y}-{m}-{d}']],
    ['project', 'parse-project']
  ]
})

const tableRef = ref()
const detailVisible = ref(false)
const currentRow = ref({})

const { crud, CRUD, columns } = useCRUD(
  {
    title: '存货转换单',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  paginate: true,
  extraHeight: 40
})

function openDetail(row) {
  currentRow.value = row
  detailVisible.value = true
}

CRUD.HOOK.handleRefresh = (crud, { data }) => {
  data.content.forEach(v => {
    v.widthThick = (v.thickness || v.width) ? (v.thickness || '-') + '*' + (v.width || '-') : '-'
  })
}
</script>

<style lang="scss" scoped>
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
.imgs-box {
  & > .el-image {
    width: 50px;
    height: 40px;
    border: 2px solid #dcdfe6;
    border-radius: 6px;
    background-color: white;
    cursor: pointer;
    + .el-image {
      margin-left: -40px;
    }
  }
}
</style>
