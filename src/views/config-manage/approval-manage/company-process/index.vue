<template>
  <div class="app-container">
    <!--工具栏-->
    <mHeader class="head-container" />
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      :data-format="dataFormat"
      return-source-data
      style="width: 100%"
    >
      <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
      <el-table-column v-if="columns.visible('name')" key="name" prop="name" show-overflow-tooltip label="审批名称" min-width="120" />
      <el-table-column v-if="columns.visible('process')" key="process" prop="process" show-overflow-tooltip label="审批流程" min-width="200" />
      <el-table-column v-if="columns.visible('node')" key="node" prop="node" show-overflow-tooltip label="抄送节点" width="120" align="center" />
      <el-table-column v-if="columns.visible('ccUserName')" key="ccUserName" prop="ccUserName" show-overflow-tooltip label="抄送人" min-width="160" />
      <el-table-column v-if="columns.visible('updateTime')" key="updateTime" prop="updateTime" show-overflow-tooltip label="更新时间"  align="center"  width="130" />
      <!--编辑-->
      <el-table-column
        v-if="checkPermission(permission.edit)"
        label="操作"
        width="130px"
        align="center"
        fixed="right"
      >
        <template v-slot="scope">
          <ud-operation :show-del="false" :data="scope.row" />
        </template>
      </el-table-column>
    </common-table>
    <mForm />
  </div>
</template>

<script setup>
import crudApi from '@/api/config/approval-config/company-process'
import { ref } from 'vue'

import { ddApproveTypeEnum, ddApprovalPositionEnum, ddTaskActionTypeEnum } from '@enum-ms/dd'

import { companyProcessConfigPM as permission } from '@/page-permission/config'
import checkPermission from '@/utils/system/check-permission'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import udOperation from '@crud/UD.operation'
import mHeader from './module/header'
import mForm from './module/form'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const dataFormat = ref([
  ['updateTime', 'parse-time']
])
const { crud, columns, CRUD } = useCRUD(
  {
    title: '公司审批流程',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    invisibleColumns: [],
    hasPagination: false
  },
  tableRef
)

const { maxHeight } = useMaxHeight()

// 处理数据
CRUD.HOOK.handleRefresh = (crud, data) => {
  data.data.content = data.data.map((v) => {
    v.name = ddApproveTypeEnum.VL?.[v.type]
    v.node = ddApprovalPositionEnum.VL?.[v.ccPosition]
    v.process = v.approvers?.map(v => `【${ddTaskActionTypeEnum?.VL[v.type]}：${v.userName}】`).join('→')
    return v
  })
}
</script>
