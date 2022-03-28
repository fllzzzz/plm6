<template>
  <common-drawer
    ref="drawerRef"
    :visible="crud.detailVisible"
    :content-loading="crud.detailLoading"
    :before-close="crud.cancelDetail"
    :title="drawerTitle"
    :show-close="true"
    size="80%"
    custom-class="mat-label-print-receipt-mode-detail"
  >
    <template #titleAfter>
      <!-- <title-after-info :order="order" :detail="detail" /> -->
    </template>
    <template #titleRight>
      <common-button class="filter-item" type="success" size="mini" icon="el-icon-printer" @click="toBatchPrint">批量打印</common-button>
      <el-select class="select-mini" v-model="crud.props.copies" placeholder="可选择打印份数" size="mini" style="width: 100px">
        <el-option v-for="i in 3" :key="`copies_${i}`" :label="`打印${i}份`" :value="i" />
      </el-select>
    </template>
    <template #content>
      <common-table
        ref="tableRef"
        :data="detail.materialList"
        :data-format="columnsDataFormat"
        :max-height="maxHeight"
        show-summary
        :expand-row-keys="expandRowKeys"
        @selection-change="handleSelectionChange"
        row-key="id"
      >
        <el-expand-table-column :data="detail.materialList" v-model:expand-row-keys="expandRowKeys" row-key="id" fixed="left">
          <template #default="{ row }">
            <expand-secondary-info :basic-class="row.basicClass" :row="row" show-graphics :show-batch-no="false" />
          </template>
        </el-expand-table-column>
        <el-table-column type="selection" width="55" align="center" fixed="left" />
        <el-table-column label="序号" type="index" align="center" width="55" fixed="left">
          <template #default="{ row, $index }">
            <!-- 是否已打印 -->
            <table-cell-tag :show="row.printedNumber >= row.number" name="已打印" type="printed" />
            <span>{{ $index + 1 }}</span>
          </template>
        </el-table-column>

        <!-- 基础信息 -->
        <material-base-info-columns :basic-class="crud.query.basicClass" fixed="left" :show-index="false" />
        <!-- 单位及其数量 -->
        <material-unit-quantity-columns :basic-class="crud.query.basicClass" />
        <!-- 次要信息 -->
        <material-secondary-info-columns :basic-class="crud.query.basicClass" />
        <!-- 仓库信息 -->
        <warehouse-info-columns />
        <el-table-column
          key="printNumber"
          :show-overflow-tooltip="true"
          prop="printNumber"
          label="批量打印数量"
          align="center"
          fixed="right"
          min-width="120"
        >
          <template #default="{ row }">
            <common-input-number v-model="row.printNumber" :min="1" :max="999" />
          </template>
        </el-table-column>
        <!--打印-->
        <el-table-column label="操作" width="70px" align="center" fixed="right">
          <template #default="{ row }">
            <material-print-button
              v-bind="$attrs"
              :material="row"
              :number="row.printNumber"
              :copies="crud ? crud.props.copies : 1"
              submit-print-record
              @printed-success="reloadDetail"
            />
          </template>
        </el-table-column>
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { computed, watch, ref, defineEmits } from 'vue'
import { receiptTypeEnum } from '@/utils/enum/modules/wms'
import { matClsEnum } from '@/utils/enum/modules/classification'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { materialColumns } from '@/utils/columns-format/wms'

import { regDetail } from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'

import usePrint from '../../composables/use-print'
import elExpandTableColumn from '@comp-common/el-expand-table-column.vue'
import materialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import materialUnitQuantityColumns from '@/components-system/wms/table-columns/material-unit-quantity-columns/index.vue'
import materialSecondaryInfoColumns from '@/components-system/wms/table-columns/material-secondary-info-columns/index.vue'
import warehouseInfoColumns from '@/components-system/wms/table-columns/warehouse-info-columns/index.vue'
import expandSecondaryInfo from '@/components-system/wms/table-columns/expand-secondary-info/index.vue'
import MaterialPrintButton from '@/components-system/wms/material-print-button.vue'

const emit = defineEmits(['printed-success'])

const drawerRef = ref()
const tableRef = ref()
const expandRowKeys = ref([])
const selections = ref([])
// 表格列数据格式转换
const columnsDataFormat = ref([...materialColumns])
const { print } = usePrint({ emit })
const { CRUD, crud, detail } = regDetail()

// 表格高度处理
const { maxHeight } = useMaxHeight(
  {
    mainBox: '.mat-label-print-receipt-mode-detail',
    extraBox: ['.el-drawer__header'],
    wrapperBox: ['.el-drawer__body'],
    clientHRepMainH: true,
    minHeight: 300,
    extraHeight: 10
  },
  () => computed(() => !crud.detailLoading)
)

// 标题
const drawerTitle = computed(() => {
  if (detail && receiptTypeEnum.V[detail.receiptType]) {
    return `${receiptTypeEnum.V[detail.receiptType].DOC}：${detail.serialNumber}`
  } else {
    return ''
  }
})

// 打开详情前
CRUD.HOOK.beforeToDetail = () => {
  selections.value = []
}

// 详情加载后
CRUD.HOOK.beforeDetailLoaded = async (crud, detail) => {
  await setSpecInfoToList(detail.materialList)
  detail.materialList = await numFmtByBasicClass(detail.materialList, {
    toSmallest: false,
    toNum: false
  })
  detail.materialList.forEach((row) => {
    if (row.basicClass === matClsEnum.STEEL_COIL.V) {
      row.number = 1
    } else {
      row.number = row.quantity
    }
    // 需打印数量 = 数量 - 已打印数量
    row.printNumber = row.number - (row.printedNumber || 0)
    // 需要打印数量至少为1
    // row.printNumber = row.printNumber > 0 ? row.printNumber : 1
    if (row.printNumber > 0) {
      const trigger = watch(tableRef, (newVal) => {
        if (newVal) {
          tableRef.value.toggleRowSelection(row, true)
          setTimeout(() => {
            trigger()
          }, 0)
        }
      })
    } else {
      row.printNumber = 1
    }
  })
}

// 重载详情
function reloadDetail() {
  selections.value = []
  crud.reloadDetail()
}

// 批量打印
async function toBatchPrint() {
  await print(selections.value, crud.props.copies)
  crud.cancelDetail()
  crud.refresh()
}

// 选择框change
function handleSelectionChange(val) {
  console.log('val', val)
  selections.value = val
}
</script>

<style lang="scss" scoped>
.mat-label-print-receipt-mode-detail {
  .el-table {
    ::v-deep(td .cell) {
      min-height: 28px;
      line-height: 28px;
    }
  }
}
</style>
