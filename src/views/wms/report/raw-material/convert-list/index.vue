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
      <el-table-column key="project" prop="project" v-if="columns.visible('project')" label="项目名称" align="left" min-width="150" show-overflow-tooltip />
      <el-table-column key="classifyName" prop="classifyName" v-if="columns.visible('classifyName')" label="物料名称" align="center" show-overflow-tooltip width="100" />
      <el-table-column key="quantity" prop="quantity" v-if="columns.visible('quantity')" label="转换总长度(m)" align="right" show-overflow-tooltip />
      <el-table-column key="mete" prop="mete" v-if="columns.visible('mete')" label="转换总重(kg))" align="right" show-overflow-tooltip />
      <el-table-column key="amountExcludingVat" v-if="columns.visible('amountExcludingVat')" prop="amountExcludingVat" label="转换总金额(不含税)" align="right" show-overflow-tooltip />
      <el-table-column key="date" prop="date" v-if="columns.visible('date')" label="转化时间" align="center" show-overflow-tooltip min-width="150" />
      <!--编辑与删除-->
      <el-table-column
        v-if="checkPermission([...permission.list?.get])"
        label="操作"
        width="80px"
        align="center"
      >
        <template v-slot="scope">
          <common-button icon="el-icon-view" type="primary" size="mini" @click="openDetail(scope.row)" v-permission="permission.list?.get"/>
        </template>
      </el-table-column>
    </common-table>
    <common-drawer title="条板转换列表" v-model="detailVisible" size="95%">
      <template #titleAfter>
        <el-tag>{{currentRow.project?.id?'项目：'+projectNameFormatter(currentRow.project):'公共库'}}{{}}</el-tag>
      </template>
      <template #content>
        <convert-list :showType="'coilPlate'" :currentInfo="currentRow" />
      </template>
    </common-drawer>
  <!--分页组件-->
  <!-- <pagination /> -->
  </div>
</template>

<script setup>
import { convertListReport as get } from '@/api/wms/report/raw-material/convert-list'
import { ref, computed } from 'vue'

import { projectNameFormatter } from '@/utils/project'
import { parseTime } from '@/utils/date'
import checkPermission from '@/utils/system/check-permission'
import { reportRawMaterialConvertListPM as permission } from '@/page-permission/wms'
import { DP } from '@/settings/config'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
// import pagination from '@crud/Pagination'
import mHeader from './module/header'
import convertList from './module/list/index'
// import mDetail from './module/detail'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const dataFormat = computed(() => {
  return [
    ['amountExcludingVat', ['to-thousand', DP.YUAN]],
    ['project', 'parse-project']
  ]
})

const tableRef = ref()
const detailVisible = ref(false)
const currentRow = ref({})

const { crud, CRUD, columns } = useCRUD(
  {
    title: '存货转换单报表',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { get },
    hasPagination: false,
    dataPath: ''
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  paginate: true,
  extraHeight: 40
})

function openDetail(row) {
  currentRow.value = row?.sourceRow
  detailVisible.value = true
}

CRUD.HOOK.handleRefresh = async (crud, { data }) => {
  await setSpecInfoToList(data)
  data = await numFmtByBasicClass(data, { toSmallest: false, toNum: true })
  data.forEach(v => {
    v.date = (v.startTime && v.endTime) ? parseTime(v.startTime, '{y}-{m}-{d}') + '~' + parseTime(v.endTime, '{y}-{m}-{d}') : undefined
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
