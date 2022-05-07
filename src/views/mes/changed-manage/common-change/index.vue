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
      :data-format="dataFormat"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      row-key="id"
      style="width: 100%"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column
        v-if="columns.visible('createTime')"
        prop="createTime"
        :show-overflow-tooltip="true"
        label="变更时间"
        width="130"
        align="center"
      >
        <template #default="{ row }">
          <span>{{ row.createTime }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('userName')" key="userName" prop="userName" :show-overflow-tooltip="true" label="变更人">
        <template #default="{ row }">
          <span>{{ row.userName }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('changTypeText')"
        prop="changTypeText"
        :show-overflow-tooltip="true"
        label="变更原因"
        align="center"
      >
        <template #default="{ row }">
          <span>{{ row.changTypeText }}</span>
        </template>
      </el-table-column>
      <belonging-info-columns :columns="columns" showProject showMonomer showArea />
      <el-table-column
        v-if="
          columns.visible('artifactSerialNumber') &&
          crud.query.productType & (componentTypeEnum.MACHINE_PART.V | componentTypeEnum.ASSEMBLE.V)
        "
        key="artifactSerialNumber"
        prop="artifactSerialNumber"
        :show-overflow-tooltip="true"
        label="构件编号"
        min-width="110"
      >
        <template #default="{ row }">
          <span>{{ row.artifactSerialNumber }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('assembleSerialNumber') && crud.query.productType & componentTypeEnum.MACHINE_PART.V"
        key="assembleSerialNumber"
        prop="assembleSerialNumber"
        :show-overflow-tooltip="true"
        label="组立编号"
        min-width="110"
      >
        <template #default="{ row }">
          <span>{{ row.assembleSerialNumber }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('serialNumber')"
        key="serialNumber"
        prop="serialNumber"
        :show-overflow-tooltip="true"
        label="编号"
        min-width="110"
      >
        <template #default="{ row }">
          <span>{{ row.serialNumber }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('oldQuantity')"
        key="oldQuantity"
        prop="oldQuantity"
        :show-overflow-tooltip="true"
        label="原清单数量"
        width="100"
        align="center"
      >
        <template #default="{ row }">
          <span>{{ row.oldQuantity }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('newQuantity')"
        key="newQuantity"
        prop="newQuantity"
        :show-overflow-tooltip="true"
        label="变更后清单数量"
        width="110"
        align="center"
      >
        <template #default="{ row }">
          <span>{{ row.newQuantity }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('totalInProductionQuantity')"
        key="totalInProductionQuantity"
        prop="totalInProductionQuantity"
        :show-overflow-tooltip="true"
        label="进入生产数量"
        width="100"
        align="center"
      >
        <template #default="{ row }">
          <span>{{ row.totalInProductionQuantity }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('status')" :show-overflow-tooltip="true" prop="status" label="状态" align="center" width="90">
        <template #default="{ row }">
          <el-tag :type="abnormalHandleStatusEnum.V[row.status].TAG">{{ abnormalHandleStatusEnum.VL[row.status] }}</el-tag>
        </template>
      </el-table-column>
      <el-table-column v-permission="[...permission.save, ...permission.detail]" label="操作" width="160px" align="center" fixed="right">
        <template #default="{ row }">
          <span
            v-if="
              row.type & abnormalHandleTypeEnum.MACHINE_PART.V &&
              !(row.status & (abnormalHandleStatusEnum.PROCESSING_COMPLETE.V | abnormalHandleStatusEnum.CANCEL.V))
            "
          >
            <common-button size="mini" type="primary" v-permission="[...permission.save]" @click="partHandle(row)"> 处理 </common-button>
          </span>
          <span
            v-if="
              row.type & abnormalHandleTypeEnum.MACHINE_PART.V &&
              row.status & (abnormalHandleStatusEnum.PROCESSING_COMPLETE.V | abnormalHandleStatusEnum.CANCEL.V)
            "
          >
            <span>-</span>
          </span>
          <span v-if="!(row.type & abnormalHandleTypeEnum.MACHINE_PART.V)">
            <common-button
              size="mini"
              v-if="!(row.status & (abnormalHandleStatusEnum.PROCESSING_COMPLETE.V | abnormalHandleStatusEnum.CANCEL.V))"
              type="primary"
              v-permission="[...permission.save]"
              @click="toHandle(row)"
            >
              处理
            </common-button>
            <common-button v-permission="[...permission.detail]" size="mini" type="info" @click="toDetail(row)">查看</common-button>
          </span>
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <schedule-handle-drawer v-model:visible="scheduleHandleVisible" :info="detailInfo" @success="crud.toQuery" />
    <schedule-detail-drawer v-model:visible="scheduleDetailVisible" :info="detailInfo" />
    <schedule-detail-preview-drawer v-model:visible="scheduleDetailPreviewVisible" :info="detailInfo" />
    <production-handle-drawer v-model:visible="productionHandleVisible" :info="detailInfo" @success="crud.toQuery" />
    <production-detail-drawer v-model:visible="productionDetailVisible" :info="detailInfo" />
    <production-detail-preview-drawer v-model:visible="productionDetailPreviewVisible" :info="detailInfo" />
  </div>
</template>

<script setup>
import crudApi from '@/api/mes/changed-manage/common'
import { changeStatus, partChange } from '@/api/mes/changed-manage/artifact'
import { ElMessageBox } from 'element-plus'
import { changeListPM as permission } from '@/page-permission/mes'

import { ref } from 'vue'
import { abnormalHandleStatusEnum, abnormalHandleTypeEnum, componentTypeEnum } from '@enum-ms/mes'
import { deepClone } from '@data-type/index'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import belongingInfoColumns from '@comp-mes/table-columns/belonging-info-columns'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import productionHandleDrawer from './module/production-handle-drawer.vue'
import scheduleHandleDrawer from './module/schedule-handle-drawer.vue'
import scheduleDetailPreviewDrawer from './module/schedule-detail-preview-drawer.vue'
import scheduleDetailDrawer from './module/schedule-detail-drawer.vue'
import productionDetailPreviewDrawer from './module/production-detail-preview-drawer'
import productionDetailDrawer from './module/production-detail-drawer'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const dataFormat = ref([['createTime', 'parse-time']])

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '变更管理',
    permission: { ...permission },
    sort: [],
    invisibleColumns: [],
    optShow: { ...optShow },
    crudApi: { ...crudApi }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

const productionHandleVisible = ref(false)
const scheduleHandleVisible = ref(false)
const scheduleDetailVisible = ref(false)
const scheduleDetailPreviewVisible = ref(false)
const productionDetailVisible = ref(false)
const productionDetailPreviewVisible = ref(false)
const detailInfo = ref({})

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    v.changTypeText = v.changeType
    v.handleType = v.type
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
        const handleVisible = _handleType & abnormalHandleTypeEnum.SCHEDULE_CHANGE.V ? scheduleHandleVisible : productionHandleVisible
        openDrawer(handleVisible, row)
      } catch (error) {
        console.log('仍要处理', error)
      }
    })
  } else {
    const handleVisible = row.handleType & abnormalHandleTypeEnum.SCHEDULE_CHANGE.V ? scheduleHandleVisible : productionHandleVisible
    openDrawer(handleVisible, row)
  }
}

function partHandle(row) {
  ElMessageBox.confirm('是否确认处理', '提示', {
    confirmButtonText: '确认',
    cancelButtonText: '取消',
    type: 'warning'
  }).then(async () => {
    try {
      await partChange(row.id)
      crud.refresh()
    } catch (error) {
      console.log('零件处理', error)
    }
  })
}

function toDetail(row) {
  const detailVisible =
    row.handleType & abnormalHandleTypeEnum.SCHEDULE_CHANGE.V
      ? row.status & abnormalHandleStatusEnum.PROCESSING_COMPLETE.V
        ? scheduleDetailVisible
        : scheduleDetailPreviewVisible
      : row.status & abnormalHandleStatusEnum.PROCESSING_COMPLETE.V
        ? productionDetailVisible
        : productionDetailPreviewVisible
  openDrawer(detailVisible, row)
}

function openDrawer(visible, row) {
  detailInfo.value = deepClone(row)
  visible.value = true
}
</script>
