<template>
  <div class="app-container">
    <!--工具栏-->
    <mHeader ref="header" :permission="permission" />
    <!--表格渲染-->
    <common-table ref="tableRef" v-loading="crud.loading" :data="crud.data" :max-height="maxHeight" style="width: 100%">
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column v-if="columns.visible('name')" key="name" :show-overflow-tooltip="true" prop="name" label="名称" align="center" />
      <el-table-column
        v-if="columns.visible('symbol')"
        key="symbol"
        :show-overflow-tooltip="true"
        prop="symbol"
        label="符号"
        align="center"
      />
      <el-table-column v-if="columns.visible('type')" :show-overflow-tooltip="true" prop="type" label="类型" align="center">
        <template v-slot="scope">
          {{ unitTypeEnum.VL[scope.row.type] }}
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('enabled')" :show-overflow-tooltip="true" prop="enabled" label="状态" align="center">
        <template v-slot="scope">
          <template v-if="useCheckPermission(permission.edit)">
            <el-switch :disabled="updateLoading" v-model="scope.row.enabled" class="drawer-switch" @change="handleChangeEnabled(scope.row, scope.row.enabled)" />
          </template>
          <template v-else>
            {{ enabledEnum.VL[scope.row.enabled] }}
          </template>
        </template>
      </el-table-column>
      <!--编辑与删除-->
      <el-table-column v-permission="permission.edit" label="操作" width="130px" align="center">
        <template v-slot="scope">
          <div>
            <udOperation :show-del="!scope.row.boolSystem" :show-edit="false" :data="scope.row" />
          </div>
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <mForm />
  </div>
</template>

<script setup>
// 系统字段添加标识
import crudApi, { editEnabled } from '@/api/config/main/unit-config'
import { ref } from 'vue'
import { enabledEnum, unitTypeEnum } from '@enum-ms/common'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import useCheckPermission from '@compos/use-check-permission'
import udOperation from '@crud/UD.operation'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import mForm from './module/form'

// // crud交由presenter持有
const permission = {
  get: ['config_unitConfig:get'],
  edit: ['config_unitConfig:edit'],
  del: ['config_unitConfig:del'],
  add: ['config_unitConfig:add']
}

const optShow = {
  batchAdd: true,
  add: false,
  edit: false,
  del: false,
  download: false
}

const updateLoading = ref(false)
const tableRef = ref()
const { CRUD, crud, columns } = useCRUD(
  {
    title: '基础单位配置',
    formStore: true,
    formStoreKey: 'BASE_CONFIG_UNIT',
    sort: ['id.desc'],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

// 启用状态变更
async function handleChangeEnabled(data, val) {
  try {
    updateLoading.value = true
    await editEnabled({ id: data.id, enabled: val })
    crud.refresh()
    crud.notify(enabledEnum.VL[val] + '成功', CRUD.NOTIFICATION_TYPE.SUCCESS)
  } catch (error) {
    console.log('基础单位-操作使用状态', error)
    data.enabled = data.enabled === enabledEnum.TRUE.V ? enabledEnum.FALSE.V : enabledEnum.TRUE.V
  } finally {
    updateLoading.value = false
  }
}
</script>
