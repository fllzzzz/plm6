<template>
  <div class="app-container">
    <div class="head-container">
      <mHeader />
    </div>
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      row-key="id"
      style="width: 100%"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <belonging-info-columns :columns="columns" showProject />
      <!-- <el-table-column
        v-if="columns.visible('createTime')"
        key="createTime"
        prop="createTime"
        :show-overflow-tooltip="true"
        label="变更时间"
        width="170"
        align="center"
      >
        <template v-slot="scope">
          <span v-parse-time="'{y}-{m}-{d} {h}:{i}'">{{ scope.row.createTime }}</span>
        </template>
      </el-table-column> -->
      <el-table-column v-if="columns.visible('userName')" key="userName" prop="userName" :show-overflow-tooltip="true" label="变更人">
        <template v-slot="scope">
          <span v-empty-text>{{ scope.row.userName }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('serialNumber')"
        key="serialNumber"
        prop="serialNumber"
        :show-overflow-tooltip="true"
        label="编号"
      >
        <template v-slot="scope">
          <span v-empty-text>{{ scope.row.serialNumber }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('oldQuantity')"
        key="oldQuantity"
        prop="oldQuantity"
        :show-overflow-tooltip="true"
        label="数量"
        align="center"
      >
        <template v-slot="scope">
          <span v-empty-text>{{ scope.row.oldQuantity }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('totalInProductionQuantity')"
        key="totalInProductionQuantity"
        prop="totalInProductionQuantity"
        :show-overflow-tooltip="true"
        label="已进入生产"
        align="center"
      >
        <template v-slot="scope">
          <span v-empty-text>{{ scope.row.totalInProductionQuantity }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('changTypeText')"
        key="changTypeText"
        prop="changTypeText"
        :show-overflow-tooltip="true"
        label="变更类型"
        align="center"
      >
        <template v-slot="scope">
          <span v-empty-text>{{ scope.row.changTypeText }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('newQuantity')"
        key="newQuantity"
        prop="newQuantity"
        :show-overflow-tooltip="true"
        label="变更后数量"
        align="center"
      >
        <template v-slot="scope">
          <span v-empty-text>{{ scope.row.newQuantity }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('status')" :show-overflow-tooltip="true" prop="status" label="状态" align="center" width="100">
        <template #default="{ row }">
          <el-tag :type="abnormalHandleStatusEnum.V[row.status].TAG">{{ abnormalHandleStatusEnum.VL[row.status] }}</el-tag>
        </template>
      </el-table-column>
      <el-table-column v-permission="[...permission.save, ...permission.detail]" label="操作" width="160px" align="center" fixed="right">
        <template v-slot="scope">
          <common-button
            size="mini"
            v-if="!(scope.row.status & (abnormalHandleStatusEnum.PROCESSING_COMPLETE.V | abnormalHandleStatusEnum.CANCEL.V))"
            type="primary"
            v-permission="[...permission.save]"
            @click="toHandle(scope.row)"
            >处理</common-button
          >
          <common-button v-permission="[...permission.detail]" size="mini" type="info" @click="toDetail(scope.row)">查看</common-button>
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <handle-drawer v-model:visible="handleVisible" :info="detailInfo" @success="crud.toQuery" />
    <detail-drawer v-model:visible="detailVisible" :info="detailInfo" />
  </div>
</template>

<script setup>
import crudApi from '@/api/mes/changed-manage/common'
import { changeStatus } from '@/api/mes/changed-manage/artifact'
import { reactive, ref, provide } from 'vue'
import { ElMessageBox } from 'element-plus'

import { abnormalHandleStatusEnum, abnormalChangeTypeEnum } from '@enum-ms/mes'
import EO from '@/utils/enum'
import { changeListPM as permission } from '@/page-permission/mes'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import belongingInfoColumns from '@comp-mes/table-columns/belonging-info-columns'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import handleDrawer from './module/handle-drawer'
import detailDrawer from './module/detail-drawer'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '变更管理',
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

const handleVisible = ref(false)
const detailVisible = ref(false)
let detailInfo = reactive({})

// 构件变更处理方式
const handleMethodEnum = {
  DECREASE_TASK: {
    K: 'DECREASE_TASK',
    L: '多余任务处理',
    V: 0,
    COLUMNS: [
      // { label: '类型', field: 'type', width: '', preview: true },
      { label: '任务数', field: 'taskQuantity', width: '150px', align: 'center', preview: false }
    ]
  },
  EXCEPTION_HANDLE: {
    K: 'EXCEPTION_HANDLE',
    L: '异常处理',
    V: 1,
    COLUMNS: [
      { label: '工序', field: 'processName', width: '', preview: true },
      { label: '类型', field: 'reportTypeText', width: '', preview: true },
      { label: '生产数量', field: 'quantity', width: '150px', align: 'center', preview: false }
    ]
  }
}
provide('handleMethodEnum', handleMethodEnum)
provide('handleMethodEnumV', EO.key2val(handleMethodEnum))

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    v.changTypeText = abnormalChangeTypeEnum.VL[v.changeType]
    // 未生产数总和 = 任务数量 - 已生产数量
    v.unproducedMete = v.totalTaskQuantity - v.totalInProductionQuantity || 0
    // 需要减少的任务数 = 任务数量 - 变更后的数量
    v.needDecreaseTaskMete = v.totalTaskQuantity - v.newQuantity || 0
    // 条件一: 未生产数总和 >= 需要减少的任务数 => 进行减少任务操作(处理总数=需要减少的任务数)
    if (v.unproducedMete >= v.needDecreaseTaskMete) {
      v.handleType = handleMethodEnum.DECREASE_TASK.V
      v.canHandleTotalMete = v.needDecreaseTaskMete
    }
    // 条件二: 未生产数总和 < 需要减少的任务数 => 进行异常处理（可报废或二次利用）操作
    if (v.unproducedMete < v.needDecreaseTaskMete) {
      v.handleType = handleMethodEnum.EXCEPTION_HANDLE.V
      // 异常处理总数 = 已生产数量 - 变更后的数量
      v.canHandleTotalMete = v.totalInProductionQuantity - v.newQuantity
    }
    return v
  })
}

function toHandle(row) {
  if (row.status === abnormalHandleStatusEnum.PENDING.V) {
    ElMessageBox.confirm('处理异常前请通知车间将相关构件进行异常上报后再进行异常处理！', '提示', {
      confirmButtonText: '仍要处理',
      cancelButtonText: '取消',
      type: 'warning'
    }).then(async () => {
      try {
        const _handleType = await changeStatus(row.id)
        row.handleType = _handleType
        row.status = abnormalHandleStatusEnum.PROCESSING.V
        openDrawer(handleVisible, row)
      } catch (error) {
        console.log('仍要处理', error)
      }
    })
  } else {
    openDrawer(handleVisible, row)
  }
}

function toDetail(row) {
  openDrawer(detailVisible, row)
}

function openDrawer(visible, row) {
  visible.value = true
  detailInfo = Object.assign(detailInfo, row)
}
</script>
