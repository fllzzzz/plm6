<template>
  <div class="app-container">
    <!--工具栏-->
    <m-header />
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :max-height="maxHeight"
      style="width: 100%"
      @selection-change="crud.selectionChangeHandler"
      row-key="id"
    >
      <el-table-column type="selection" align="center" width="55" />
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column
        v-if="columns.visible('serialNumber')"
        key="serialNumber"
        :show-overflow-tooltip="true"
        prop="serialNumber"
        label="编码"
        align="left"
        width="150"
      >
        <template #default="{ row }">
          <factory-table-cell-tag :id="row.factoryId" />
          <span>{{ row.serialNumber }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('classifyFullName')"
        key="classifyFullName"
        :show-overflow-tooltip="true"
        prop="classifyFullName"
        label="科目"
        align="left"
        min-width="180"
      />
      <el-table-column
        v-if="columns.visible('specification')"
        key="specification"
        :show-overflow-tooltip="true"
        prop="specification"
        label="规格"
        align="left"
        min-width="150"
      />
      <el-table-column v-if="columns.visible('unit')" :show-overflow-tooltip="true" prop="unit" label="单位" align="left" width="120">
        <template #default="{ row }">
          <el-tag :type="row.unitType === measureTypeEnum.MEASURE.V ? '' : 'warning'" style="margin-right: 8px" effect="plain">
            {{ measureTypeEnum.VL[row.unitType] }}
          </el-tag>
          {{ row.unit }}
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('minimumInventory')"
        key="minimumInventory"
        prop="minimumInventory"
        label="预警数量"
        align="center"
        min-width="180"
      >
        <template #default="{ row }">
          <template v-if="row.editMode">
            <div class="edit-item">
              <common-input-number
                v-model="row.minimumInventory"
                :disabled="row.editLoading"
                :max="999999"
                :min="0"
                :precision="0"
                controls-position="right"
                label="描述文字"
                size="small"
              />
              <common-button class="icon-button" size="mini" icon="el-icon-circle-close" type="warning" @click="cancelRowEdit(row)" />
            </div>
          </template>
          <span v-else>{{ row.minimumInventory }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('enabled')"
        :show-overflow-tooltip="true"
        prop="enabled"
        label="启用状态"
        align="center"
        width="100"
      >
        <template #default="{ row }">
          <template v-if="checkPermission(permission.edit)">
            <el-switch
              :disabled="row.enabledLoading"
              v-model="row.enabled"
              class="drawer-switch"
              @change="handleEnabledChange(row, ['classifyFullName', 'specification'])"
            />
          </template>
          <template v-else>
            {{ enabledEnum.VL[row.enabled] }}
          </template>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('updateTime')" key="updateTime" prop="updateTime" label="编辑日期" width="140px">
        <template #default="{ row }">
          <span>{{ parseTime(row.updateTime) }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('createTime')" key="createTime" prop="createTime" label="创建日期" width="140px">
        <template #default="{ row }">
          <span>{{ parseTime(row.createTime) }}</span>
        </template>
      </el-table-column>
      <!--编辑与删除-->
      <el-table-column v-permission="permission.edit" label="操作" width="130px" align="center">
        <template #default="{ row }">
          <common-button
            v-if="row.editMode"
            :loading="row.editLoading"
            type="success"
            size="mini"
            icon="el-icon-check"
            @click="confirmRowEdit(row)"
          />
          <common-button v-else type="primary" size="mini" icon="el-icon-edit" @click="row.editMode = !row.editMode" />
          <udOperation :show-edit="false" :data="row" />
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <m-batch-form />
  </div>
</template>

<script setup>
import crudApi, { editEnabled, editMinimumInventory } from '@/api/wms/inventory-warning'
import { rawMaterialInventoryWarningPM as permission } from '@/page-permission/wms'

import { ref } from 'vue'
import { enabledEnum } from '@enum-ms/common'
import { measureTypeEnum } from '@enum-ms/wms'
import { parseTime } from '@/utils/date'
import checkPermission from '@/utils/system/check-permission'

import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import useCrudEnabledChange from '@compos/use-crud-enabled-change'
import useRowEdit from '@compos/use-row-edit'
import factoryTableCellTag from '@comp-base/factory-table-cell-tag.vue'
import pagination from '@crud/Pagination'
import udOperation from '@crud/UD.operation'
import mHeader from './module/header'
import mBatchForm from './module/batch-form'
import { numFmtByBasicClass, numFmtByUnit } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'

const optShow = {
  batchAdd: true,
  add: false,
  edit: false,
  del: true,
  download: false
}

const editField = ['minimumInventory']

const tableRef = ref()
const { CRUD, crud, columns } = useCRUD(
  {
    title: '库存预警',
    sort: ['id.desc'],
    formStore: true,
    formStoreKey: 'WMS_INVENTORY_WARNING',
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })
const { handleEnabledChange } = useCrudEnabledChange({ CRUD, crud, editEnabled })

const { rowInit, cancelRowEdit, confirmRowEdit } = useRowEdit(editMinimumInventory, editField, editDataFormat)

CRUD.HOOK.handleRefresh = async (crud, { data }) => {
  await setSpecInfoToList(data.content)
  data.content = await numFmtByBasicClass(data.content, {
    toSmallest: false,
    toNum: true
  })
  data.content.forEach((row) => {
    row.unit = row.unitType === measureTypeEnum.MEASURE.V ? row.measureUnit : row.accountingUnit
  })
  rowInit(data.content)
}

// 修改格式转换
async function editDataFormat(row, data) {
  await numFmtByUnit(data, {
    unit: row.unitType === measureTypeEnum.MEASURE.V ? row.measureUnit : row.accountingUnit,
    precision: row.unitType === measureTypeEnum.MEASURE.V ? row.measurePrecision : row.accountingPrecision,
    fields: ['minimumInventory'],
    toSmallest: true,
    toNum: true
  })
  return data
}
</script>

<style lang="scss" scoped>
.icon-button {
  margin-left: 10px;
  padding: 7px;
}

.edit-item {
  display: inline-flex;
}
</style>
