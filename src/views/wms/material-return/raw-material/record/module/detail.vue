<template>
  <common-drawer
    ref="drawerRef"
    :visible="crud.detailVisible"
    :content-loading="crud.detailLoading"
    :before-close="crud.cancelDetail"
    :title="drawerTitle"
    :show-close="true"
    size="100%"
    custom-class="raw-mat-return-application-record-detail"
  >
    <template #titleAfter>
      <title-after-info :detail="detail" />
    </template>
    <template #titleRight>
      <!-- TODO:打印按钮 -->
      <el-tag
        v-if="isNotBlank(detail.reviewStatus)"
        :type="reviewStatusEnum.V[detail.reviewStatus].TAG"
        size="medium"
        style="margin-right: 10px"
      >
        {{ reviewStatusEnum.VL[detail.reviewStatus] }}
      </el-tag>
    </template>
    <template #content>
      <material-info class="material-info" :basic-class="detail.basicClass" :material="currentSource" />
      <common-table
        ref="tableRef"
        :data="detail.list"
        :max-height="maxHeight"
        show-summary
        :summary-method="getSummaries"
        :expand-row-keys="expandRowKeys"
        row-key="id"
        highlight-current-row
        @row-click="handleRowClick"
      >
        <el-expand-table-column :data="detail.list" v-model:expand-row-keys="expandRowKeys" row-key="id" fixed="left">
          <template #default="{ row }">
            <expand-secondary-info :basic-class="row.basicClass" :row="row" :show-batch-no="false" show-remark show-graphics />
          </template>
        </el-expand-table-column>
        <!-- 基础信息 -->
        <material-base-info-columns :basic-class="detail.basicClass" fixed="left" />
        <!-- 次要信息 -->
        <material-secondary-info-columns :basic-class="detail.basicClass" />
        <!-- 单位及其数量 -->
        <material-unit-quantity-columns :basic-class="detail.basicClass" />
        <!-- 仓库信息 -->
        <warehouse-info-columns show-project />
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { computed, ref } from 'vue'
import { reviewStatusEnum } from '@/utils/enum/modules/common'
import { tableSummary } from '@/utils/el-extra'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { isNotBlank } from '@/utils/data-type'

import { regDetail } from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import useMatBaseUnit from '@/composables/store/use-mat-base-unit'
import ElExpandTableColumn from '@comp-common/el-expand-table-column.vue'
import MaterialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import MaterialUnitQuantityColumns from '@/components-system/wms/table-columns/material-unit-quantity-columns/index.vue'
import MaterialSecondaryInfoColumns from '@/components-system/wms/table-columns/material-secondary-info-columns/index.vue'
import WarehouseInfoColumns from '@/components-system/wms/table-columns/warehouse-info-columns/index.vue'
import ExpandSecondaryInfo from '@/components-system/wms/table-columns/expand-secondary-info/index.vue'
import MaterialInfo from '@/views/wms/material-return/raw-material/application/components/material-info/index.vue'
import titleAfterInfo from '@/views/wms/material-return/raw-material/components/title-after-info.vue'

const drawerRef = ref()
const tableRef = ref()
const expandRowKeys = ref([])
// 当前退库源数据
const currentSource = ref()
const { CRUD, crud, detail } = regDetail()

const { baseUnit } = useMatBaseUnit() // 当前分类基础单位

// 表格高度处理
const { maxHeight } = useMaxHeight(
  {
    mainBox: '.raw-mat-return-application-record-detail',
    extraBox: ['.el-drawer__header', '.material-info'],
    wrapperBox: ['.el-drawer__body'],
    clientHRepMainH: true,
    minHeight: 300,
    extraHeight: 10
  },
  () => computed(() => !crud.detailLoading)
)

// 标题
const drawerTitle = computed(() => {
  if (detail && detail.serialNumber) {
    return `退库单：${detail.serialNumber}`
  } else {
    return '退库单'
  }
})

CRUD.HOOK.beforeDetailLoaded = async (crud, detail) => {
  // 当前数据
  currentSource.value = undefined
  await setSpecInfoToList(detail.list)
  await numFmtByBasicClass(detail.list, { toNum: true })
  const sourceList = detail.list.map((v) => v.source)
  await setSpecInfoToList(sourceList)
  await numFmtByBasicClass(
    sourceList,
    { toNum: true },
    {
      mete: ['mete', 'returnableMete', 'singleMete', 'singleReturnableMete']
    }
  )
}

// 行选中
function handleRowClick(row, column, event) {
  currentSource.value = row.source
}

// 合计
function getSummaries(param) {
  // 获取单位精度
  const dp =
    detail.basicClass && baseUnit.value && baseUnit.value[detail.basicClass] ? baseUnit.value[detail.basicClass].measure.precision : 0
  return tableSummary(param, { props: [['quantity', dp], 'mete'] })
}
</script>

<style lang="scss" scoped>
.raw-mat-return-application-record-detail {
  .material-info {
    margin-bottom: 10px;
  }
  .el-drawer__header .el-tag {
    min-width: 70px;
    text-align: center;
  }
  .el-table {
    ::v-deep(.cell) {
      height: 28px;
      line-height: 28px;
    }

    ::v-deep(.current-row > td.el-table__cell) {
      --el-table-current-row-background-color: #d7ffef;
    }
  }
}
</style>
