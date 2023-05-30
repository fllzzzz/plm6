<template>
  <div class="app-container">
    <mHeader />
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      row-key="id"
      style="width: 100%"
      @selection-change="crud.selectionChangeHandler"
    >
      <el-table-column type="selection" width="55" align="center" />
      <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
      <el-table-column
        v-if="columns.visible('name')"
        align="center"
        key="name"
        prop="name"
        :show-overflow-tooltip="true"
        label="设备名称"
      />
      <el-table-column
        v-if="columns.visible('num')"
        align="center"
        key="num"
        prop="num"
        :show-overflow-tooltip="true"
        label="数量（台）"
      />
      <el-table-column
        v-if="columns.visible('originalValue')"
        align="center"
        key="originalValue"
        prop="originalValue"
        :show-overflow-tooltip="true"
        label="单台初始价值"
      />
      <el-table-column
        v-if="columns.visible('depreciationYear')"
        align="center"
        key="depreciationYear"
        prop="depreciationYear"
        :show-overflow-tooltip="true"
        label="折旧年限"
      />
      <el-table-column
        v-if="columns.visible('yearDepreciationRate')"
        align="center"
        key="yearDepreciationRate"
        prop="yearDepreciationRate"
        :show-overflow-tooltip="true"
        label="年折旧率"
      >
        <template #default="{ row }">
          <span>{{ (row.yearDepreciationRate * 100).toFixed(2) }} %</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('yearDepreciationAmount')"
        align="center"
        key="yearDepreciationAmount"
        prop="yearDepreciationAmount"
        :show-overflow-tooltip="true"
        label="年折旧额"
      />
      <el-table-column
        v-if="columns.visible('monthDepreciationRate')"
        align="center"
        key="monthDepreciationRate"
        prop="monthDepreciationRate"
        :show-overflow-tooltip="true"
        label="月折旧率"
      >
        <template #default="{ row }">
          <span>{{ (row.monthDepreciationRate * 100).toFixed(2) }} %</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('monthDepreciationAmount')"
        align="center"
        key="monthDepreciationAmount"
        prop="monthDepreciationAmount"
        :show-overflow-tooltip="true"
        label="月折旧额"
      />
      <el-table-column
        v-if="columns.visible('startDepreciation')"
        align="center"
        key="startDepreciation"
        prop="startDepreciation"
        :show-overflow-tooltip="true"
        label="是否开始折旧"
      >
        <template #default="{ row }">
        <el-tag v-if="row.startDepreciation" type="success" size="medium" effect="plain">是</el-tag>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('boolStatus')"
        align="center"
        key="boolStatus"
        prop="boolStatus"
        :show-overflow-tooltip="true"
        label="状态"
      >
        <template #default="{ row }">
          <el-switch
            v-model="row.boolStatus"
            :active-value="enabledEnum.TRUE.V"
            :inactive-value="enabledEnum.FALSE.V"
            :disabled="!checkPermission(permission.changeStatus)"
            class="drawer-switch"
            @change="changeStatus(row, row.boolStatus)"
          />
        </template>
      </el-table-column>
      <el-table-column v-if="checkPermission([...permission.edit, ...permission.del])" align="center" label="操作" width="140">
        <template v-slot="{ row }">
          <udOperation :data="row" />
        </template>
      </el-table-column>
    </common-table>
    <!-- 分页 -->
    <pagination />
    <!-- 表单 -->
    <m-form />
  </div>
</template>
<script setup>
import { ref } from 'vue'
import crudApi, { editStatus } from '@/api/contract/expense-entry/device-depreciation'

import { contractDeviceDepreciationPM as permission } from '@/page-permission/contract'
import checkPermission from '@/utils/system/check-permission'
import { ElMessageBox, ElNotification } from 'element-plus'
import { enabledEnum } from '@enum-ms/common'

import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import pagination from '@crud/Pagination'
import udOperation from '@crud/UD.operation'
import mForm from './module/form.vue'
import mHeader from './module/header.vue'

const optShow = {
  add: true,
  edit: false,
  del: true,
  download: false
}

const tableRef = ref()
const { crud, columns } = useCRUD(
  {
    title: '设备折旧',
    sort: [],
    optShow: { ...optShow },
    permission: { ...permission },
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

async function changeStatus(data, val) {
  try {
    await ElMessageBox.confirm(`此操作将 ${data.name} 的状态改为 ${enabledEnum.VL[val]}, 是否继续？`, '提示', {
      confirmButtonText: '确定',
      cancelButtonText: '取消',
      type: 'warning'
    })
    await editStatus({ id: data.id, boolStatus: val })
    crud.toQuery()
    ElNotification({ title: `${data.name}的状态修改成功`, type: 'success', duration: 3000 })
  } catch (error) {
    console.log('设备折旧状态变更失败', error)
    data.boolStatus = data.boolStatus === enabledEnum.TRUE.V ? enabledEnum.FALSE.V : enabledEnum.TRUE.V
  }
}
const { maxHeight } = useMaxHeight({
  paginate: true
})
</script>
