<template>
  <common-drawer
    ref="drawerRef"
    :visible="crud.detailVisible"
    :content-loading="!clsLoaded || crud.detailLoading"
    :before-close="crud.cancelDetail"
    :title="detailTitle"
    :show-close="true"
    :size="hasAssocPreparation ? '100%' : '620px'"
    custom-class="purchase-order-raw-mat-detail"
  >
    <template v-if="isNotBlank(detail)" #titleAfter>
      <el-tag v-if="detail.boolPartyA" type="danger" effect="dark" size="small">甲供</el-tag>
      <el-tag
        :type="boolPurchaseCompleted ? 'success' : detail.boolUsed ? 'warning' : 'info'"
        :effect="boolPurchaseCompleted ? 'dark' : 'plain'"
        size="small"
      >
        <span v-parse-enum="{ e: purchaseStatusEnum, v: detail.purchaseStatus }" />
        <span v-if="detail.boolUsed && detail.purchaseStatus === purchaseStatusEnum.UNFINISHED.V"> (有入库)</span>
      </el-tag>
      <el-tag
        :type="boolSettled ? 'success' : 'info'"
        :effect="boolSettled ? 'dark' : 'plain'"
        size="small"
        v-parse-enum="{ e: settlementStatusEnum, v: detail.settlementStatus }"
      />
    </template>
    <template #content>
      <div class="main-content">
        <el-form v-if="isNotBlank(detail)" :model="detail" size="small" label-position="left" label-width="100px">
          <div class="form-content" :style="heightStyle">
            <div class="form-left">
              <el-form-item label="物料种类" prop="basicClass">
                <div class="flex-rss child-mr-10">
                  <span v-parse-enum="{ e: baseMaterialTypeEnum, v: detail.purchaseType, extra: '：' }" />
                  <span v-parse-enum="{ e: matClsEnum, v: detail.basicClass, bit: true, split: ' | ' }" />
                </div>
              </el-form-item>

              <el-form-item
                v-if="detail.basicClass & matClsEnum.MATERIAL.V"
                label="辅材明细"
                prop="auxMaterialNames"
                style="width: 100%; word-break: break-all"
              >
                <span v-if="detail.auxMaterialIds.includes(0)">所有辅材</span>
                <span v-else v-split="detail.auxMaterialNames" />
              </el-form-item>

              <el-form-item
                v-if="detail.basicClass & matClsEnum.OTHER.V"
                label="其它明细"
                prop="otherMaterialNames"
                style="width: 100%; word-break: break-all"
              >
                <span v-if="detail.otherMaterialIds.includes(0)">所有其它科目</span>
                <span v-else v-split="detail.otherMaterialNames" />
              </el-form-item>

              <template v-if="detail.supplyType === orderSupplyTypeEnum.SELF.V">
                <el-form-item label="合同量" prop="mete">
                  <span>{{ detail.mete }} {{ detail.meteUnit }}</span>
                  <!-- 【<span v-parse-enum="{ e: weightMeasurementModeEnum, v: detail.weightMeasurementMode }" />】 -->
                </el-form-item>

                <el-form-item label="合同额" prop="amount">
                  <span>{{ detail.amount }} 元</span>
                  【<span v-parse-enum="{ e: invoiceTypeEnum, v: detail.invoiceType }" />
                  <span v-if="detail.invoiceType !== invoiceTypeEnum.RECEIPT.V">（{{ detail.taxRate }}%）</span>】
                </el-form-item>

                <!-- <el-form-item label="发票及税率" prop="invoiceType"> </el-form-item> -->
                <el-form-item label="订单类型" prop="purchaseOrderPaymentMode">
                  <span v-parse-enum="{ e: purchaseOrderPaymentModeEnum, v: detail.purchaseOrderPaymentMode }" />
                </el-form-item>
              </template>

              <el-form-item prop="weightMeasurementMode" label="计量方式">
                <span v-parse-enum="{ e: weightMeasurementModeEnum, v: detail.weightMeasurementMode }" />
              </el-form-item>

              <el-form-item label="物流信息" prop="logistics">
                <span v-parse-enum="{ e: logisticsTransportTypeEnum, v: detail.logisticsTransportType }" />
                （费用<span v-parse-enum="{ e: logisticsPayerEnum, v: detail.logisticsPayerType }" />）
              </el-form-item>

              <el-form-item label="供应商" prop="supplier">
                <span v-if="detail.supplier">{{ detail.supplier.name }}</span>
              </el-form-item>

              <el-form-item label="签订主体" prop="branchCompanyName" style="width: 100%">
                <span v-if="detail.branchCompany">{{ detail.branchCompany.name }}</span>
              </el-form-item>

              <el-form-item label="创建人" prop="applicantName">
                <span v-empty-text>{{ detail.applicantName }}</span>
                （创建时间：<span v-parse-time="detail.createTime" />）
              </el-form-item>
              <el-form-item label="最后操作人" prop="lastOperatorName">
                <span v-empty-text>{{ detail.lastOperatorName }}</span>
                （操作时间：<span v-parse-time="detail.userUpdateTime" />）
              </el-form-item>

              <el-form-item label="备注" prop="remark" style="width: 100%">
                <span style="word-break: break-all">{{ detail.remark }}</span>
              </el-form-item>
              <!-- <el-form-item label="申购单号" prop="requisitionsSN">
              <span class="pre-wrap">{{ detail.requisitionsSN ? detail.requisitionsSN.join(`\n`) : '' }}</span>
            </el-form-item> -->
              <el-form-item label="关联项目" class="el-form-item-4" prop="projectIds" style="width: 100%">
                <span v-if="baseMaterialTypeEnum.RAW_MATERIAL.V === detail.purchaseType" class="pre-wrap">
                  {{ detail.projects ? detail.projects.map((v) => projectNameFormatter(v, null, false)).join(`\n`) : '' }}
                </span>
              </el-form-item>

              <el-divider><span class="title">附件</span></el-divider>
              <upload-list
                show-download
                showView
                :file-classify="fileClassifyEnum.PURCHASE_ORDER_ATT.V"
                v-model:files="detail.attachments"
                :uploadable="false"
                empty-text="暂未上传采购合同附件"
              />
            </div>
            <template v-if="hasAssocPreparation">
              <div class="vertical-dashed-divider" />
              <div class="form-right">
                <div class="right-head flex-rbs">
                  <!-- 关联备料单-->
                  <span class="assoc-preparation-list">
                    <span class="label">关联备料单</span>
                    <el-tag
                      v-for="serialNumber in detail.preparationSNList"
                      :key="serialNumber"
                      :effect="currentPreparationSN === serialNumber ? 'dark' : 'plain'"
                      class="preparation-sn-tag"
                      @click="currentPreparationChange(serialNumber)"
                    >
                      {{ serialNumber }}
                    </el-tag>
                  </span>
                </div>
                <!-- 清单列表 -->
                <common-table
                  ref="tableRef"
                  :data="preparationList"
                  :max-height="maxHeight - 150"
                  :default-expand-all="false"
                  highlight-current-row
                  row-key="id"
                >
                  <el-table-column label="序号" type="index" align="center" width="60" fixed="left" />
                  <el-table-column
                    v-if="!currentPreparationSN"
                    key="preparationSNList"
                    show-overflow-tooltip
                    prop="preparationSNList"
                    label="备料单号"
                    min-width="140px"
                    fixed="left"
                  >
                    <template #default="{ row }">
                      <span v-split="row.preparationSNList" />
                    </template>
                  </el-table-column>
                  <!-- 基础信息 -->
                  <material-base-info-columns spec-merge fixed="left" :show-index="false" />
                  <!-- 次要信息 -->
                  <material-secondary-info-columns fixed="left" />
                  <!-- 单位及其数量 TODO: 编辑 -->
                  <material-unit-quantity-columns />
                </common-table>
              </div>
            </template>
          </div>
        </el-form>
      </div>
    </template>
  </common-drawer>
</template>

<script setup>
import { watch, nextTick, computed, ref } from 'vue'
import { matClsEnum } from '@enum-ms/classification'
import { orderSupplyTypeEnum, baseMaterialTypeEnum, purchaseOrderPaymentModeEnum, purchaseStatusEnum } from '@enum-ms/wms'
import { weightMeasurementModeEnum, invoiceTypeEnum, settlementStatusEnum } from '@enum-ms/finance'
import { logisticsPayerEnum, logisticsTransportTypeEnum } from '@/utils/enum/modules/logistics'
import { fileClassifyEnum } from '@enum-ms/file'
import { projectNameFormatter } from '@/utils/project'
import { deepClone, isNotBlank } from '@/utils/data-type'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'

import { regDetail } from '@compos/use-crud'
import uploadList from '@comp/file-upload/UploadList.vue'
import useMatClsList from '@/composables/store/use-mat-class-list'
import useMaxHeight from '@/composables/use-max-height'

import MaterialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import MaterialUnitQuantityColumns from '@/components-system/wms/table-columns/material-unit-quantity-columns/index.vue'
import MaterialSecondaryInfoColumns from '@/components-system/wms/table-columns/material-secondary-info-columns/index.vue'

const { CRUD, crud, detail } = regDetail()

const { loaded: clsLoaded, rawMatClsKV } = useMatClsList()

// 表格高度处理
const { maxHeight, heightStyle } = useMaxHeight(
  {
    mainBox: '.purchase-order-raw-mat-detail',
    extraBox: ['.el-drawer__header'],
    wrapperBox: ['.el-drawer__body'],
    clientHRepMainH: true
  },
  () => crud.detailVisible
)

// 标题
const detailTitle = computed(() => {
  if (isNotBlank(detail) && detail.serialNumber) {
    return `订单：${detail.serialNumber}`
  } else {
    return `订单`
  }
})

// 是否有关联备料单
const hasAssocPreparation = computed(() => {
  return detail.preparationSNList && detail.preparationSNList.length > 0
})

// 当前选中的备料单号
const currentPreparationSN = ref()
// 备料列表map
const preparationListMap = ref(new Map())
// 合并的备料列表
const mergePreparationList = ref([])
// 当前展示的备料列表
const preparationList = computed(() => {
  let list = []
  if (currentPreparationSN.value) {
    list = preparationListMap.value.get(currentPreparationSN.value)
  } else {
    list = mergePreparationList.value
  }
  return list || []
})

// 是否
const boolPurchaseCompleted = computed(() => detail.purchaseStatus === purchaseStatusEnum.FINISHED.V)
const boolSettled = computed(() => detail.settlementStatus === settlementStatusEnum.SETTLED.V)

// 详情加载
CRUD.HOOK.beforeDetailLoaded = (crud, detail) => {
  init()
  const setInfo = async () => {
    if (detail.auxMaterialIds) {
      detail.auxMaterialNames = detail.auxMaterialIds.map((id) => {
        const material = rawMatClsKV.value[id]
        if (material) {
          return material.name
        }
        return '-'
      })
    }
    if (detail.otherMaterialIds) {
      detail.otherMaterialNames = detail.otherMaterialIds.map((id) => {
        const material = rawMatClsKV.value[id]
        if (material) {
          return material.name
        }
        return '-'
      })
    }
    // 是否甲供
    detail.boolPartyA = detail.supplyType === orderSupplyTypeEnum.PARTY_A.V
    if (Array.isArray(detail.preparationList) && detail.preparationList.length > 0) {
      // 获取规格信息以及单位转换
      await setSpecInfoToList(detail.preparationList)
      await numFmtByBasicClass(detail.preparationList, {
        toNum: true
      })
      // 遍历设置备料相关信息
      detail.preparationList.forEach((row) => {
        const list = preparationListMap.value.get(row.preparationSN)
        if (list) {
          // 存在该备料单数组，则推入数组
          list.push(row)
        } else {
          // 不存在该备料单数组，则创建该备料单数组
          preparationListMap.value.set(row.preparationSN, [row])
        }
        // 查询相同物料
        const mpIndex = mergePreparationList.value.findIndex((mp) => {
          // 钢板 | TODO: 辅材、还有属性
          // 钢板：编号（科目-规格）、颜色、品牌
          return mp.serialNumber === row.serialNumber && mp.color === row.color && mp.brand === row.brand
        })
        if (mpIndex > -1) {
          // 已存在物料，则数量累加
          mergePreparationList.value[mpIndex].quantity += row.quantity
          mergePreparationList.value[mpIndex].mete += row.mete
          mergePreparationList.value[mpIndex].preparationSNList.push(row.preparationSN)
        } else {
          // 未存在物料，则将物料推入数组
          const cloneRow = deepClone(row)
          cloneRow.preparationSNList = [row.preparationSN]
          mergePreparationList.value.push(cloneRow)
        }
      })
    }
  }
  if (!clsLoaded.value) {
    const trigger = watch(
      clsLoaded,
      async (val) => {
        if (val) {
          await setInfo()
          nextTick(() => {
            trigger()
          })
        }
      },
      { immediate: true }
    )
  } else {
    setInfo()
  }
}

// 初始化
function init() {
  currentPreparationSN.value = undefined
  preparationListMap.value = new Map()
  mergePreparationList.value = []
}

// 当前选中的备料单号
function currentPreparationChange(preparationSN) {
  if (preparationSN !== currentPreparationSN.value) {
    currentPreparationSN.value = preparationSN
  } else {
    currentPreparationSN.value = undefined
  }
}
</script>

<style lang="scss" scoped>
::-webkit-scrollbar {
  /*滚动条整体样式*/
  width: 4px; /*高宽分别对应横竖滚动条的尺寸*/
  height: 4px;
}
.form-content {
  display: flex;
  flex-direction: row;
  justify-content: flex-start;
  align-items: flex-start;

  .form-left {
    width: 580px;
    flex: none;
    padding-left: 4px;
    padding-right: 20px;
    overflow: auto;
    .el-form-item--small.el-form-item {
      margin-bottom: 10px;
    }
    flex-wrap: wrap;
  }

  .form-right {
    width: 100%;
    height: 100%;
    overflow-y: auto;
    overflow-x: hidden;
    .right-head {
      height: 45px;
      margin-top: 5px;
      .assoc-preparation-list {
        display: block;
        .label {
          font-weight: bold;
          font-size: 15px;
          margin-right: 10px;
          color: var(--el-text-color-regular);
        }
        .preparation-sn-tag {
          user-select: none;
          min-width: 150px;
          margin-right: 10px;
          text-align: center;
          cursor: pointer;
        }
      }
    }
  }
}
</style>
