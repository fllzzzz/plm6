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
      <el-table-column prop="project.shortName" label="所属项目" min-width="250px">
        <template v-slot="scope">
          <span class="project-name">{{ projectNameFormatter(scope.row.project) }}</span>
        </template>
      </el-table-column>
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
          <span>{{ parseTime(scope.row.createTime) }}</span>
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
          <span>{{ emptyTextFormatter(scope.row.userName) }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('serialNumber')"
        key="serialNumber"
        prop="serialNumber"
        :show-overflow-tooltip="true"
        label="组立编号"
      >
        <template v-slot="scope">
          <span>{{ emptyTextFormatter(scope.row.serialNumber) }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('oldQuantity')"
        key="oldQuantity"
        prop="oldQuantity"
        :show-overflow-tooltip="true"
        label="组立总数"
        align="center"
      >
        <template v-slot="scope">
          <span>{{ emptyTextFormatter(scope.row.oldQuantity) }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('producedQuantity')"
        key="producedQuantity"
        prop="producedQuantity"
        :show-overflow-tooltip="true"
        label="已生产量"
        align="center"
      >
        <template v-slot="scope">
          <span>{{ emptyTextFormatter(scope.row.producedQuantity) }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('usedQuantity')"
        key="usedQuantity"
        prop="usedQuantity"
        :show-overflow-tooltip="true"
        label="已使用量"
        align="center"
      >
        <template v-slot="scope">
          <span>{{ emptyTextFormatter(scope.row.usedQuantity) }}</span>
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
          <span>{{ emptyTextFormatter(scope.row.changTypeText) }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('newQuantity')"
        key="newQuantity"
        prop="newQuantity"
        :show-overflow-tooltip="true"
        label="还需组立数"
        align="center"
      >
        <template v-slot="scope">
          <span>{{ emptyTextFormatter(scope.row.newQuantity) }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('canHandleTotalMete')"
        key="canHandleTotalMete"
        prop="canHandleTotalMete"
        :show-overflow-tooltip="true"
        label="可修改任务数"
        align="center"
      >
        <template v-slot="scope">
          <span>{{ emptyTextFormatter(scope.row.canHandleTotalMete) }}</span>
        </template>
      </el-table-column>
      <el-table-column v-permission="[...permission.edit, ...permission.del]" label="操作" width="160px" align="center" fixed="right">
        <template v-slot="scope">
          <common-button size="mini" type="primary" @click="toHandle(scope.row)">处理</common-button>
          <common-button size="mini" type="info" @click="toDetail(scope.row)">查看</common-button>
        </template>
      </el-table-column>
    </common-table>
    <handle-drawer v-model:visible="handleVisible" :info="detailInfo" @refresh="crud.toQuery"/>
    <detail-drawer v-model:visible="detailVisible" :info="detailInfo" />
  </div>
</template>

<script setup>
import crudApi from '@/api/mes/changed-manage/common'
import { reactive, ref } from 'vue'

import { abnormalChangeTypeEnum } from '@enum-ms/mes'
import { emptyTextFormatter } from '@data-type/index'
import { projectNameFormatter } from '@/utils/project'
import { parseTime } from '@/utils/date'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
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
    title: '组立变更',
    sort: [],
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

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data = res.data.map((v) => {
    v.changTypeText = abnormalChangeTypeEnum.VL[v.changeType]
    v.canHandleTotalMete = v.newQuantity - v.producedQuantity
    return v
  })
}

function toHandle(row) {
  openDrawer(handleVisible, row)
}

function toDetail(row) {
  openDrawer(detailVisible, row)
}

function openDrawer(visible, row) {
  visible.value = true
  detailInfo = Object.assign(detailInfo, row)
}
</script>
