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
      />
      <el-table-column
        v-if="columns.visible('commitUser.name')"
        key="commitUser.name"
        prop="commitUser.name"
        :show-overflow-tooltip="true"
        label="变更人"
        align="center"
      />
      <el-table-column
        v-if="columns.visible('reasonName')"
        prop="reasonName"
        :show-overflow-tooltip="true"
        label="变更原因"
        align="center"
      />
      <el-table-column v-if="columns.visible('project.name')" key="project.name " prop="project.name" :show-overflow-tooltip="true" label="所属项目" min-width="180px" />
      <!-- <el-table-column
        v-if="columns.visible('monomer.name')"
        prop="monomer.name"
        :show-overflow-tooltip="true"
        label="单体"
        min-width="100px"
      /> -->
      <el-table-column v-if="columns.visible('plan.name')" :show-overflow-tooltip="true" prop="plan.name" label="区域" min-width="100px" />
      <el-table-column v-if="columns.visible('name')" :show-overflow-tooltip="true" prop="name" label="名称" min-width="90px" />
      <el-table-column
        v-if="columns.visible('serialNumber')"
        prop="serialNumber"
        :show-overflow-tooltip="true"
        label="编号"
        min-width="100"
      />
      <el-table-column
        v-if="columns.visible('plate')"
        prop="plate"
        :show-overflow-tooltip="true"
        label="版型"
        min-width="110"
      />
      <el-table-column v-if="columns.visible('oldQuantity')" prop="oldQuantity" label="原清单数量" align="center" :show-overflow-tooltip="true" min-width="90" />
      <el-table-column v-if="columns.visible('newQuantity')" prop="newQuantity" label="变更后清单数量" align="center" :show-overflow-tooltip="true" min-width="90" />
      <el-table-column
        v-if="columns.visible('handleUser.name')"
        prop="handleUser.name"
        :show-overflow-tooltip="true"
        label="处理人"
        width="110"
        align="center"
      />
      <el-table-column
        v-if="columns.visible('handleTime')"
        prop="handleTime"
        :show-overflow-tooltip="true"
        label="处理时间"
        width="130"
        align="center"
      />
      <el-table-column label="状态" width="140px" align="center" fixed="right">
        <template #default="{ row: { sourceRow: row } }">
          <span
            v-if="row.handleStatusEnum && row.handleStatusEnum === changeHandleStatusEnum.PENDING.V && checkPermission(permission.audit)"
          >
            <common-button size="mini" type="success" @click="toHandle(row, changeHandleStatusEnum.PASS.V)"> 通过 </common-button>
            <common-button size="mini" type="danger" @click="toHandle(row, changeHandleStatusEnum.REJECT.V)"> 驳回 </common-button>
          </span>
          <el-tag :type="changeHandleStatusEnum.V[row.handleStatusEnum].TAG" effect="plain" v-else>
            {{ changeHandleStatusEnum.VL[row.handleStatusEnum] }}
          </el-tag>
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
  </div>
</template>
<script setup>
import crudApi, { enclosureChangeReview } from '@/api/enclosure/production-manage/change-approval'
import { ElMessageBox } from 'element-plus'
import { changeRecordPM as permission } from '@/page-permission/mes'
import checkPermission from '@/utils/system/check-permission'

import { ref } from 'vue'
import { changeHandleStatusEnum } from '@enum-ms/mes'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const dataFormat = ref([
  ['createTime', 'parse-time'],
  ['handleTime', 'parse-time']
  // ['project', 'parse-project']
])

const tableRef = ref()
const { crud, columns } = useCRUD(
  {
    title: '变更审批',
    permission: { ...permission },
    sort: [],
    invisibleColumns: [],
    optShow: { ...optShow },
    dataPath: '',
    crudApi: { ...crudApi }
  }
)

const { maxHeight } = useMaxHeight({ paginate: true })

function toHandle(row, status) {
  ElMessageBox.confirm(`是否确认${changeHandleStatusEnum.VL[status]}`, '提示', {
    confirmButtonText: '确认',
    cancelButtonText: '取消',
    type: 'warning'
  }).then(async () => {
    try {
      await enclosureChangeReview({ id: row.id, handleStatusEnum: status })
      row.handleStatusEnum = status
      crud.toQuery()
    } catch (error) {
      console.log('取消', error)
    }
  })
}

</script>
