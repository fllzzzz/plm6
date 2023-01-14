<template>
  <div class="app-container">
    <div class="header-container">
      <mHeader />
    </div>
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      row-key="id"
      style="width: 100%"
    >
      <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
      <el-table-column v-if="columns.visible('name')" align="center" key="name" prop="name" :show-overflow-tooltip="true" label="名称" />
      <el-table-column
        v-if="columns.visible('originalValue')"
        align="center"
        key="originalValue"
        prop="originalValue"
        :show-overflow-tooltip="true"
        label="原值"
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
            class="drawer-switch"
            :disabled="!checkPermission(permission.changeStatus)"
            @change="changeStatus(row, row.boolStatus)"
          />
        </template>
      </el-table-column>
      <el-table-column align="center" label="操作">
        <template v-slot="scope">
          <udOperation :data="scope.row" :disabledEdit="!scope.row.boolStatus" :disabledDel="!scope.row.boolStatus" :permission="permission" />
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
import { enabledEnum } from '@enum-ms/common'
import crudApi, { editStatus } from '@/api/contract/expense-entry/fixed-assets-depreciation'

import { plantDepreciationPM as permission } from '@/page-permission/contract'
import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import { ElMessageBox, ElNotification } from 'element-plus'
import checkPermission from '@/utils/system/check-permission'

import pagination from '@crud/Pagination'
import udOperation from '@crud/UD.operation'
import mForm from './module/form.vue'
import mHeader from './module/header.vue'

const optShow = {
  add: true,
  edit: false,
  del: false,
  download: false
}
const tableRef = ref()
const { crud, CRUD, columns } = useCRUD(
  {
    title: '厂房折旧',
    sort: [],
    optShow: { ...optShow },
    permission: { ...permission },
    crudApi: { ...crudApi },
    hasPagination: true,
    formStore: true
  },
  tableRef
)

async function changeStatus(data, val) {
  console.log(data, val)
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
    console.log('变更是否固定资产折旧失败', error)
    data.boolStatus = data.boolStatus === enabledEnum.TRUE.V ? enabledEnum.FALSE.V : enabledEnum.TRUE.V
  }
}
const { maxHeight } = useMaxHeight({
  paginate: true
})

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map(v => {
    return v
  })
}
</script>
<style lang="scss" scoped>
</style>
