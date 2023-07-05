<template>
  <common-drawer
    ref="drawerRef"
    :visible="crud.detailVisible"
    :content-loading="crud.detailLoading"
    :before-close="crud.cancelDetail"
    :title="drawerTitle"
    :show-close="true"
    size="100%"
    custom-class="inbound-inspection-detail"
  >
    <template #titleAfter>
      <el-tag v-if="detail.qualityTestingEnum" :type="inspectionStatusEnum.V[detail.qualityTestingEnum].TAG" effect="plain">
        {{ inspectionStatusEnum.V[detail.qualityTestingEnum].SL }}
      </el-tag>
    </template>
    <template #titleRight>
      <el-tag v-if="detail.licensePlate" effect="plain">{{ detail.licensePlate }}</el-tag>
      <title-after-info :order="order" :detail="detail" />
    </template>
    <template #content>
      <common-table
        :data="detail.list"
        :data-format="columnsDataFormat"
        :max-height="maxHeight"
        :expand-row-keys="expandRowKeys"
        row-key="id"
      >
        <!-- <el-expand-table-column :data="detail.list" v-model:expand-row-keys="expandRowKeys" row-key="id" fixed="left">
          <template #default="{ row }">
            <expand-secondary-info :basic-class="detail.basicClass" :row="row" show-brand />
            <p>
              备注：<span>{{ row.remark }}</span>
            </p>
          </template>
        </el-expand-table-column> -->
        <!-- 基础信息 -->
        <material-base-info-columns :basic-class="detail.basicClass"/>
        <!-- 单位及其数量 -->
        <material-unit-quantity-columns :basic-class="detail.basicClass" />
        <!-- 次要信息 -->
        <material-secondary-info-columns :basic-class="detail.basicClass" />
        <!-- <el-table-column prop="requisitionsSN" label="申购单" align="left" min-width="120px" show-overflow-tooltip /> -->
        <!-- <el-table-column prop="project" label="项目" align="left" min-width="120px" show-overflow-tooltip /> -->
        <!-- <warehouse-info-columns /> -->
        <el-table-column :show-overflow-tooltip="true" prop="qualityTestingEnum" label="状态" align="center" width="110">
          <template #default="{ row }">
            <el-tag v-if="row.qualityTestingEnum" :type="inspectionDetailStatusEnum.V[row.qualityTestingEnum].TAG">
              {{ inspectionDetailStatusEnum.V[row.qualityTestingEnum].L }}
            </el-tag>
          </template>
        </el-table-column>
      </common-table>
      <el-form class="detail-other">
        <el-form-item label="质检备注：">{{ detail.qualityTestingRemark }}</el-form-item>
        <el-form-item label="质检图片/视频："> </el-form-item>
        <div class="detail-imgs-box">
          <el-image
            v-for="url in detail.attachments"
            :preview-src-list="detail.imgUrls"
            :initial-index="1"
            :key="url.id"
            :src="url.tinyImageUrl"
            lazy
          ></el-image>
        </div>
      </el-form>
    </template>
  </common-drawer>
</template>

<script setup>
import { computed, ref } from 'vue'
import { inspectionStatusEnum, inspectionDetailStatusEnum } from '@enum-ms/wms'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { materialColumns } from '@/utils/columns-format/wms'

import { invoiceTypeEnum } from '@/utils/enum/modules/finance'
import { regDetail } from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
// import elExpandTableColumn from '@comp-common/el-expand-table-column.vue'
import materialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import materialUnitQuantityColumns from '@/components-system/wms/table-columns/material-unit-quantity-columns/index.vue'
import materialSecondaryInfoColumns from '@/components-system/wms/table-columns/material-secondary-info-columns/index.vue'
// import warehouseInfoColumns from '@/components-system/wms/table-columns/warehouse-info-columns/index.vue'
// import expandSecondaryInfo from '@/components-system/wms/table-columns/expand-secondary-info/index.vue'
import titleAfterInfo from '@/views/wms/material-inbound/raw-material/components/title-after-info.vue'
import useDecimalPrecision from '@compos/store/use-decimal-precision'

const { decimalPrecision } = useDecimalPrecision()

// 表格列数据格式转换
// const columnsDataFormat = ref([...materialHasAmountColumns, ['remark', 'empty-text']])

const columnsDataFormat = computed(() => {
  return [
    ...materialColumns,
    // 金额相关
    ['invoiceType', ['parse-enum', invoiceTypeEnum, { f: 'SL' }]],
    ['taxRate', ['suffix', '%']],
    ['unitPrice', ['to-thousand', decimalPrecision.value.wms]],
    ['unitPriceExcludingVAT', ['to-thousand', decimalPrecision.value.wms]],
    ['amount', ['to-thousand', decimalPrecision.value.wms]],
    ['amountExcludingVAT', ['to-thousand', decimalPrecision.value.wms]],
    ['inputVAT', ['to-thousand', decimalPrecision.value.wms]],
    ['remark', 'empty-text']
  ]
})

const drawerRef = ref()
const expandRowKeys = ref([])
const { CRUD, crud, detail } = regDetail()

// 表格高度处理
const { maxHeight } = useMaxHeight(
  {
    mainBox: '.inbound-inspection-detail',
    extraBox: ['.el-drawer__header', '.detail-other'],
    wrapperBox: ['.el-drawer__body'],
    clientHRepMainH: true,
    minHeight: 300,
    extraHeight: 10
  },
  () => computed(() => !crud.detailLoading)
)

// 采购合同信息
const order = computed(() => detail.purchaseOrder || {})

// 标题
const drawerTitle = computed(() =>
  crud.detailLoading ? `质检详情` : `质检详情：${detail.serialNumber}（ ${order.value.supplier ? order.value.supplier.name : ''} ）`
)

CRUD.HOOK.beforeDetailLoaded = async (crud, detail) => {
  await setSpecInfoToList(detail.list)
  detail.list = await numFmtByBasicClass(detail.list, {
    toSmallest: false,
    toNum: false
  })
  detail.imgUrls = detail.attachments?.map((o) => o.imageUrl) || []
}
</script>

<style lang="scss" scoped>
.inbound-inspection-detail {
  .el-table {
    ::v-deep(td .cell) {
      min-height: 28px;
      line-height: 28px;
    }
  }
}

.detail-other {
  margin-top: 15px;
}

.detail-imgs-box {
  & > .el-image {
    width: 150px;
    height: 140px;
    border: 2px solid #dcdfe6;
    border-radius: 6px;
    background-color: white;
    cursor: pointer;
    + .el-image {
      margin-left: 10px;
    }
  }
}
</style>
