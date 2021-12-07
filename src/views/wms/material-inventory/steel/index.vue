<template>
  <div class="app-container">
    <!--工具栏-->
    <mHeader />
    <!-- 表格渲染 -->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :max-height="maxHeight"
      :default-expand-all="false"
      :expand-row-keys="expandRowKeys"
      row-key="id"
      @selection-change="crud.selectionChangeHandler"
    >
      <!-- <el-expand-table-column :data="crud.data" v-model:expand-row-keys="expandRowKeys" row-key="id" fixed="left">
        <template #default="{ row }">
          <expand-secondary-info v-if="showAmount || showWarehouse" :basic-class="basicClass" :row="row" />
        </template>
      </el-expand-table-column> -->
      <el-table-column type="selection" width="55" align="center" />
      <el-table-column label="序号" type="index" align="center" width="50" fixed="left" />
      <!-- 基础信息 -->
      <material-base-info-columns :columns="columns" :basic-class="basicClass" show-factory />
      <!-- 单位及其数量 -->
      <material-unit-quantity-columns :columns="columns" :basic-class="basicClass" :show-unit="false" />
      <!-- 次要信息 -->
      <material-secondary-info-columns :columns="columns" :basic-class="basicClass" />
      <warehouse-info-columns :columns="columns" />
      <!--编辑与删除-->
      <el-table-column label="操作" width="180px" align="center" fixed="right">
        <template #default="{ row }">
          <common-button v-permission="permission.outbound" type="primary" size="mini" @click="toOutHandle(row)">
            <svg-icon icon-class="wms-outbound" />
          </common-button>
          <common-button v-permission="permission.transfer" type="warning" size="mini" @click="toTransfer(row)">
            <svg-icon icon-class="wms-transfer" />
          </common-button>
          <common-button icon="el-icon-printer" type="success" size="mini" @click="toPrintLabel(row)"/>
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <!-- 出库办理表单 -->
    <outbound-handling-form v-model="outboundHandlingVisible" :basic-class="basicClass" :material="currentRow" />
  </div>
</template>

<script setup>
import { computed, ref } from 'vue'
import { getSteelPlateInventory } from '@/api/wms/material-inventory'
import { matClsEnum } from '@enum-ms/classification'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'

import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
// import elExpandTableColumn from '@comp-common/el-expand-table-column.vue'
// import expandSecondaryInfo from '@/components-system/wms/table-columns/expand-secondary-info/index.vue'
import materialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import materialUnitQuantityColumns from '@/components-system/wms/table-columns/material-unit-quantity-columns/index.vue'
import materialSecondaryInfoColumns from '@/components-system/wms/table-columns/material-secondary-info-columns/index.vue'
import warehouseInfoColumns from '@/components-system/wms/table-columns/warehouse-info-columns/index.vue'
import outboundHandlingForm from '@/views/wms/outbound-components/outbound-handling-form/index.vue'
import mHeader from './module/header'
import pagination from '@crud/Pagination'
import { measureTypeEnum } from '@/utils/enum/modules/wms'

// crud交由presenter持有
const permission = {
  get: ['wms_matWarehouse_steel:get'],
  outbound: ['wms_matWarehouse_steel:outbound'],
  transfer: ['wms_matWarehouse_steel:transfer']
}

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { CRUD, crud, columns } = useCRUD(
  {
    title: '入库记录',
    sort: ['id.desc'],
    invisibleColumns: ['editorName', 'userUpdateTime', 'licensePlate'],
    requiredQuery: ['basicClass'],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { get: getSteelPlateInventory }
  },
  tableRef
)

const currentRow = ref({})
const expandRowKeys = ref([])
// 出库办理显示
const outboundHandlingVisible = ref(false)

const { maxHeight } = useMaxHeight({ paginate: true })

// 基础类型
const basicClass = computed(() => crud.query.basicClass || matClsEnum.STEEL_PLATE.V)

// 处理刷新
CRUD.HOOK.handleRefresh = async (crud, { data }) => {
  await setSpecInfoToList(data.content)
  data.content = await numFmtByBasicClass(data.content, {
    toSmallest: false,
    toNum: false
  })
  data.content.forEach(v => {
    v.operableQuantity = v.quantity - v.frozenQuantity
    v.operableMete = v.mete - v.frozenMete
    if (v.outboundUnitType === measureTypeEnum.MEASURE.V) {
      // 实际在出库中使用的数量
      v.corQuantity = v.quantity
      v.corFrozenQuantity = v.frozenQuantity
      v.corOperableQuantity = v.operableQuantity
    } else {
      v.corQuantity = v.mete
      v.corFrozenQuantity = v.frozenMete
      v.corOperableQuantity = v.operableMete
    }
  })
}

// 进行出库办理
function toOutHandle(row) {
  currentRow.value = row
  outboundHandlingVisible.value = true
}
</script>
