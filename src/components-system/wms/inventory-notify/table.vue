<template>
  <common-dialog
    title="库存预警"
    v-model="visible"
    width="800px"
    :before-close="handleClose"
    :show-close="true"
    custom-class="inventory-notify-list"
    top="10vh"
  >
    <common-table ref="tableRef" v-loading="crud.loading" :data="crud.data" :max-height="maxHeight" style="width: 100%">
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
        v-if="columns.visible('classify.fullName')"
        key="classify.fullName"
        :show-overflow-tooltip="true"
        prop="classify.fullName"
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
      <el-table-column
        v-if="columns.visible('minimumInventory')"
        key="minimumInventory"
        :show-overflow-tooltip="true"
        prop="minimumInventory"
        label="预警数量"
        align="center"
        min-width="150"
      >
        <template #default="{ row }">
          <span :style="{ color: row.inventory > 0 ? '#f18121' : 'red' }" style="margin-right: 5px">
            {{
              toFixed(
                row.inventory,
                row.unitType === measureTypeEnum.MEASURE.V
                  ? row.classify.measurePrecision
                  : row.classify.accountingPrecision
              )
            }}
          </span>
          <span>
            {{ row.unitType === measureTypeEnum.MEASURE.V ? row.classify.measureUnit : row.classify.accountingUnit }}
          </span>
        </template>
      </el-table-column>
    </common-table>
    <pagination />
  </common-dialog>
</template>

<script setup>
import { fetchInventoryNotify } from '@/api/wms/common'
import { ref, defineEmits, defineProps } from 'vue'
import { measureTypeEnum } from '@enum-ms/wms'
import { toFixed } from '@/utils/data-type'

import useCRUD from '@compos/use-crud'
import useVisible from '@compos/use-visible'
import useMaxHeight from '@compos/use-max-height'
import useMatClsSpec from '@compos/store/use-mat-cls-spec'
import pagination from '@crud/Pagination'
import factoryTableCellTag from '@comp-base/factory-table-cell-tag.vue'

const emit = defineEmits(['update:modelValue'])

const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  }
})

const optShow = {
  batchAdd: false,
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { CRUD, crud, columns } = useCRUD(
  {
    title: '库存预警',
    sort: ['id.desc'],
    optShow: { ...optShow },
    crudApi: { get: fetchInventoryNotify }
  },
  tableRef
)

const { matClsSpec, fetchMatClsSpec } = useMatClsSpec()
const { visible, handleClose } = useVisible({ emit, props })
const { maxHeight } = useMaxHeight(
  {
    mainBox: '.inventory-notify-list',
    extraBox: ['.el-dialog__header'],
    wrapperBox: ['.el-dialog__body'],
    clientHRepMainH: true,
    paginate: true,
    navbar: false
  },
  visible
)

CRUD.HOOK.handleRefresh = (crud, { data: { content }}) => {
  content.forEach((v) => {
    fetchMatClsSpec(v.classifyId)
    v.classify = matClsSpec.value[v.classifyId]
  })
}
</script>

<style lang="scss" scoped>
.inventory-notify-list {
  .el-table {
    ::v-deep(.cell) {
      height: 28px;
      line-height: 28px;
    }
  }
}
</style>
