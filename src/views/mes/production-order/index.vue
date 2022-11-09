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
      :showEmptySymbol="false"
    >
      <el-table-column prop="index" label="序号" align="center" width="50" type="index" />
      <el-table-column v-if="columns.visible('createTime')" key="createTime" prop="createTime" label="立项日期" align="center" />
      <el-table-column v-if="columns.visible('project')" key="project" prop="project" label="所属项目" min-width="150" :show-overflow-tooltip="true" />
      <el-table-column v-if="columns.visible('date')" key="date" prop="date" label="项目开始~结束日期" align="center" :show-overflow-tooltip="true" min-width="150">
        <template v-slot="scope">
          <span>{{`${parseTime(scope.row.startDate,'{y}-{m}-{d}')}~${parseTime(scope.row.endDate,'{y}-{m}-{d}')}`}}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('quantity')" key="quantity" prop="quantity" label="清单量（件/吨）" align="center" :show-overflow-tooltip="true" min-width="150">
        <template v-slot="scope">
          <span v-if="scope.row.quantity && scope.row.netWeight" style="cursor:pointer;" @click="openList(scope.row)">{{`${scope.row.quantity} | ${scope.row.netWeight}`}}</span>
          <span v-else>-</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('schedulingQuantity')" key="schedulingQuantity" prop="schedulingQuantity" label="排产量（件/吨）" align="center" :show-overflow-tooltip="true" min-width="150">
        <template v-slot="scope">
          <span v-if="scope.row.schedulingQuantity && scope.row.schedulingNetWeight">{{`${scope.row.schedulingQuantity} | ${scope.row.schedulingNetWeight}`}}</span>
          <span v-else>-</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('status')" key="status" prop="status" label="状态" align="center">
        <template v-slot="scope">
          <el-tag :type="scope.row.status===scheduleStatusEnum.NOT.V?'warning':(scope.row.status===scheduleStatusEnum.COMPLETED.V?'success':'')" v-if="scope.row.status">{{ scheduleStatusEnum.VL[scope.row.status] }}</el-tag>
        </template>
      </el-table-column>
      <!--编辑与删除-->
      <el-table-column
        label="操作"
        width="120px"
        align="center"
        v-if="checkPermission([...permission.detail])"
      >
        <template v-slot="scope">
          <common-button type="primary" size="mini" @click="openDetail(scope.row)" v-if="checkPermission(permission.detail)">计划排期</common-button>
        </template>
      </el-table-column>
    </common-table>
    <fDetail v-model="detailVisible" :currentId="currentId" @success="crud.refresh" :permission="permission"/>
    <common-drawer
      append-to-body
      ref="drawerRef"
      v-model="drawerVisible"
      top="10vh"
      :before-close="()=>{
        drawerVisible=false
      }"
      title="构零件清单"
      :wrapper-closable="false"
      custom-class="artifact-tree-drawer"
      size="90%"
    >
      <template #content>
        <structure-list :drawerVisible="drawerVisible" :currentId="currentId"/>
      </template>
    </common-drawer>
  <!--分页组件-->
  <pagination />
  </div>
</template>

<script setup>
import crudApi from '@/api/mes/production-order-manage/production-order'
import { ref } from 'vue'

import { parseTime } from '@/utils/date'
import { scheduleStatusEnum } from '@enum-ms/mes'
import { DP } from '@/settings/config'
import { mesProductionOrderPM as permission } from '@/page-permission/mes'
import checkPermission from '@/utils/system/check-permission'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import fDetail from './module/detail'
import mHeader from './module/header'
import structureList from './module/structure-list'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const dataFormat = ref([
  ['createTime', ['parse-time', '{y}-{m}-{d}']],
  ['project', 'parse-project']
])

const tableRef = ref()
const detailVisible = ref(false)
const currentId = ref()
const drawerVisible = ref(false)
const drawerRef = ref()

const { crud, columns, CRUD } = useCRUD(
  {
    title: '生产订单',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    invisibleColumns: [],
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  paginate: true,
  extraHeight: 40
})

CRUD.HOOK.handleRefresh = (crud, { data }) => {
  data.content.forEach(v => {
    v.netWeight = v.totalNetWeight ? (v.totalNetWeight / 1000).toFixed(DP.COM_WT__KG) : undefined
    v.schedulingNetWeight = v.schedulingTotalNetWeight ? (v.schedulingTotalNetWeight / 1000).toFixed(DP.COM_WT__KG) : undefined
  })
}
function openList(row) {
  if (!checkPermission(permission.detailList)) {
    return
  }
  currentId.value = row.sourceRow?.project.id
  drawerVisible.value = true
}

function openDetail(row, type) {
  currentId.value = row.sourceRow?.project.id
  detailVisible.value = true
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
</style>
