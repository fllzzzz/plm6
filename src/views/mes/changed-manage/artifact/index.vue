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
      style="width: 100%"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <belonging-info-columns :columns="columns" showProject/>
      <el-table-column
        v-if="columns.visible('createTime')"
        key="createTime"
        prop="createTime"
        :show-overflow-tooltip="true"
        label="变更时间"
        width="170"
        align="center"
      >
        <template v-slot="scope">
          <span v-parse-time="'{y}-{m}-{d} {h}:{i}'">{{ scope.row.createTime}}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('userName')"
        key="userName"
        prop="userName"
        :show-overflow-tooltip="true"
        label="变更人"
      >
        <template v-slot="scope">
          <span v-empty-text>{{ scope.row.userName }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('serialNumber')"
        key="serialNumber"
        prop="serialNumber"
        :show-overflow-tooltip="true"
        label="构件编号"
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
        label="构件数量"
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
        label="已生产量"
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
      <el-table-column v-permission="[...permission.edit, ...permission.del]" label="操作" width="160px" align="center" fixed="right">
        <template v-slot="scope">
          <common-button
            size="mini"
            v-if="scope.row.status !== abnormalHandleStatusEnum.PROCESSING_COMPLETE.V"
            type="primary"
            @click="toHandle(scope.row)"
            >处理</common-button
          >
          <common-button size="mini" type="info" @click="toDetail(scope.row)">查看</common-button>
        </template>
      </el-table-column>
    </common-table>
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

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import belongingInfoColumns from '@comp-mes/table-columns/belonging-info-columns'
import mHeader from './module/header'
import handleDrawer from './module/handle-drawer'
import detailDrawer from './module/detail-drawer'

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
    title: '构件变更',
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: false,
    dataPath: ''
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: false })

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
      { label: '类型', field: 'type', width: '', preview: true },
      { label: '多余量', field: 'extraQuantity', width: '150px', align: 'center', preview: false }
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
  res.data = res.data.map((v) => {
    v.changTypeText = abnormalChangeTypeEnum.VL[v.changeType]
    // 未生产数总和 = 任务数量 - 已生产数量
    v.unproducedMete = v.taskQuantity - v.totalInProductionQuantity || 0
    // 需要减少的任务数 = 任务数量 - 变更后的数量
    v.needDecreaseTaskMete = v.taskQuantity - v.newQuantity || 0
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
