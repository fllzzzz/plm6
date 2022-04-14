<template>
  <common-drawer
    ref="drawerRef"
    :visible="crud.detailVisible"
    :content-loading="crud.detailLoading"
    :before-close="crud.cancelDetail"
    :title="drawerTitle"
    :show-close="true"
    size="100%"
    custom-class="raw-mat-inbound-application-record-detail"
  >
    <template #titleAfter>
      <el-tag v-if="detail.applicant" type="success" effect="dark">
        {{ `申请人：${detail.applicant.name} | ${detail.applicant.deptName}` }}
      </el-tag>
      <el-tag effect="plain">{{ `出库申请时间：${parseTime(detail.createTime)}` }}</el-tag>
    </template>
    <template #content>
      <common-table
        :data="detail.list"
        :data-format="columnsDataFormat"
        :max-height="maxHeight"
        show-summary
        :summary-method="getSummaries"
        :expand-row-keys="expandRowKeys"
        row-key="id"
      >
        <el-expand-table-column :data="detail.list" v-model:expand-row-keys="expandRowKeys" row-key="id" fixed="left">
          <template #default="{ row }">
            <expand-secondary-info :basic-class="row.basicClass" :row="row" show-remark show-graphics>
              <p v-if="row.boolTransfer">
                调拨：
                <span>（来源）</span>
                <span style="color: brown">{{ row.sourceProject }}</span>
                <span> ▶ </span>
                <span>（目的）</span>
                <span style="color: #3a8ee6">{{ row.project }}</span>
              </p>
            </expand-secondary-info>
          </template>
        </el-expand-table-column>
        <!-- 基础信息 -->
        <material-base-info-columns :basic-class="detail.basicClass" fixed="left" show-outbound-mode />
        <!-- 次要信息 -->
        <material-secondary-info-columns />
        <!-- 单位及其数量 -->
        <material-unit-quantity-columns />
        <!-- 仓库信息 -->
        <warehouse-info-columns show-project show-transfer />
        <el-table-column key="recipient" label="领用人" width="100px" align="center">
          <template #default="{ row }">
            <el-tooltip v-if="row.recipient" placement="top" effect="light" :content="`${row.recipient.deptName}`">
              <span>{{ row.recipient.name }}</span>
            </el-tooltip>
          </template>
        </el-table-column>
        <el-table-column key="outboundTime" prop="outboundTime" label="出库时间" width="125px" align="center" />
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { computed, ref } from 'vue'
import { tableSummary } from '@/utils/el-extra'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { parseTime } from '@/utils/date'
import { materialColumns } from '@/utils/columns-format/wms'

import { regDetail } from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import ElExpandTableColumn from '@comp-common/el-expand-table-column.vue'
import MaterialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import MaterialUnitQuantityColumns from '@/components-system/wms/table-columns/material-unit-quantity-columns/index.vue'
import MaterialSecondaryInfoColumns from '@/components-system/wms/table-columns/material-secondary-info-columns/index.vue'
import WarehouseInfoColumns from '@/components-system/wms/table-columns/warehouse-info-columns/index.vue'
import ExpandSecondaryInfo from '@/components-system/wms/table-columns/expand-secondary-info/index.vue'

const drawerRef = ref()
const expandRowKeys = ref([])
// 表格列格式化
const columnsDataFormat = ref([
  ...materialColumns,
  ['remark', 'empty-text'],
  ['project', 'parse-project'],
  ['sourceProject', 'parse-project'],
  ['outboundTime', ['parse-time', '{y}-{m}-{d} {h}:{i}:{s}']]
])
const { CRUD, crud, detail } = regDetail()

// 表格高度处理
const { maxHeight } = useMaxHeight(
  {
    mainBox: '.raw-mat-inbound-application-record-detail',
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
  if (detail && detail.applicationSN) {
    return `出库申请单：${detail.applicationSN}`
  } else {
    return '出库申请单'
  }
})

CRUD.HOOK.beforeDetailLoaded = async (crud, detail) => {
  await setSpecInfoToList(detail.list)
  detail.list = await numFmtByBasicClass(detail.list, {
    toSmallest: false,
    toNum: true
  })
}

// 合计
function getSummaries(param) {
  return tableSummary(param, { props: ['quantity', 'mete'] })
}
</script>

<style lang="scss" scoped>
.raw-mat-inbound-application-record-detail {
  .el-drawer__header .el-tag {
    min-width: 70px;
    text-align: center;
  }
  .el-table {
    ::v-deep(td .cell) {
      min-height: 28px;
      line-height: 28px;
    }
  }
}
</style>
