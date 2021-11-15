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
        <template v-slot="scope">
          <factory-table-cell-tag :id="scope.row.factoryId" />
          <span>{{ scope.row.serialNumber }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('fullClassifyName')"
        key="fullClassifyName"
        :show-overflow-tooltip="true"
        prop="fullClassifyName"
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
        <template v-slot="scope">
          <el-tag :type="scope.row.unitType === measureTypeEnum.MEASURE.V ? '' : 'warning'" style="margin-right: 8px" effect="plain">
            {{ measureTypeEnum.VL[scope.row.unitType] }}
          </el-tag>
          {{ scope.row.unit }}
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('minimumInventory')"
        key="minimumInventory"
        :show-overflow-tooltip="true"
        prop="minimumInventory"
        label="预警数量"
        align="center"
        min-width="180"
      >
        <template v-slot="scope">
          <template v-if="scope.row.editMode">
            <div class="edit-item">
              <el-input-number
                v-model="scope.row.minimumInventory"
                :disabled="scope.row.editLoading"
                :max="999999"
                :min="0"
                :precision="0"
                controls-position="right"
                label="描述文字"
                size="small"
              />
              <common-button class="icon-button" size="mini" icon="el-icon-circle-close" type="warning" @click="cancelRowEdit(scope.row)" />
            </div>
          </template>
          <span v-else>{{ scope.row.minimumInventory }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('enabled')"
        :show-overflow-tooltip="true"
        prop="enabled"
        label="状态"
        align="center"
        width="100"
      >
        <template v-slot="scope">
          <template v-if="checkPermission(permission.edit)">
            <el-switch
              :disabled="scope.row.enabledLoading"
              v-model="scope.row.enabled"
              class="drawer-switch"
              @change="handleEnabledChange(scope.row, ['fullClassifyName', 'specification'])"
            />
          </template>
          <template v-else>
            {{ enabledEnum.VL[scope.row.enabled] }}
          </template>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('updateTime')" key="updateTime" prop="updateTime" label="编辑日期" width="140px">
        <template v-slot="scope">
          <span>{{ parseTime(scope.row.updateTime) }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('createTime')" key="createTime" prop="createTime" label="创建日期" width="140px">
        <template v-slot="scope">
          <span>{{ parseTime(scope.row.createTime) }}</span>
        </template>
      </el-table-column>
      <!--编辑与删除-->
      <el-table-column v-permission="permission.edit" label="操作" width="130px" align="center">
        <template v-slot="scope">
          <common-button
            v-if="scope.row.editMode"
            :loading="scope.row.editLoading"
            type="success"
            size="mini"
            icon="el-icon-check"
            @click="confirmRowEdit(scope.row)"
          />
          <common-button v-else type="primary" size="mini" icon="el-icon-edit" @click="scope.row.editMode = !scope.row.editMode" />
          <udOperation :show-edit="false" :data="scope.row" />
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

const permission = {
  get: ['wms_inventoryWarning:get'],
  edit: ['wms_inventoryWarning:edit'],
  del: ['wms_inventoryWarning:del'],
  add: ['wms_inventoryWarning:add']
}

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

const { rowInit, cancelRowEdit, confirmRowEdit } = useRowEdit(editMinimumInventory, editField)

CRUD.HOOK.handleRefresh = (crud, { data: { content }}) => {
  rowInit(content)
}
</script>

<style lang="scss" scoped>
.icon-button {
  margin-left: 10px;
  padding: 7px;
}
</style>
