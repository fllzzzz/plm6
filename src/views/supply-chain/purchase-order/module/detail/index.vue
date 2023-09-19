<template>
  <common-drawer
    ref="drawerRef"
    :visible="crud.detailVisible"
    :content-loading="crud.detailLoading"
    :before-close="crud.cancelDetail"
    :title="detailTitle"
    :show-close="true"
    :size="isNotBlank(detail.details)?'100%':'620px'"
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
            <div class="form-left" :style="`width:${isNotBlank(detail.details)?'450px':'100%'}`">
              <el-form-item label="物料种类" prop="basicClass">
                <div class="flex-rss child-mr-10">
                  <span
                    v-if="!(detail.materialType & materialPurchaseClsEnum.MATERIAL.V)"
                    v-parse-enum="{ e: materialPurchaseClsEnum, v: detail.materialType, extra: '：' }"
                  />
                  <span v-parse-enum="{ e: matClsEnum, v: detail.basicClass, bit: true, split: ' | ' }" />
                </div>
              </el-form-item>

              <!-- <el-form-item
                v-if="detail.basicClass & matClsEnum.MATERIAL.V"
                label="辅材明细"
                prop="auxMaterialNames"
                style="width: 100%; word-break: break-all"
              >
                <span v-if="detail.auxMaterialIds.includes(0)">所有辅材</span>
                <span v-else v-split="detail.auxMaterialNames" />
              </el-form-item> -->

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

              <el-form-item prop="weightMeasurementMode" label="计量方式" v-if="isBlank(detail.details)">
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
                <span class="pre-wrap">
                  {{ detail.projects ? detail.projects.map((v) => projectNameFormatter(v, null, false)).join(`\n`) : '' }}
                </span>
              </el-form-item>

              <el-divider><span class="title">附件</span></el-divider>
              <upload-list
                show-download
                :file-classify="fileClassifyEnum.PURCHASE_ORDER_ATT.V"
                v-model:files="detail.attachments"
                :uploadable="false"
                empty-text="暂未上传采购合同附件"
              />
            </div>
            <template v-if="isNotBlank(detail.details)">
              <div class="vertical-dashed-divider" />
              <div class="form-right">
                <div class="right-head flex-rbs">
                  <span class="right-head-content">
                    <span class="label">{{ boolUseRequisitions ? '关联申购单号' : '采购清单' }}</span>
                    <el-tag v-for="item in detail.applyPurchase" :key="item.id" effect="plain" class="preparation-sn-tag">
                      {{ item.serialNumber }}
                    </el-tag>
                  </span>
                </div>
                <!-- 清单列表 -->
                <detail-table :material-type="detail.materialType" :list="detail.details" :max-height="maxHeight - 150" :bool-use-requisitions="boolUseRequisitions"/>
                <div class="table-remark">
                  <span class="title">合同量</span>
                  <span class="con">{{ detail.mete }} {{ detail.meteUnit }}</span>
                  <span class="title">合同额</span>
                  <span class="con">{{ detail.amount }} 元</span>
                </div>
              </div>
            </template>
          </div>
        </el-form>
      </div>
    </template>
  </common-drawer>
</template>

<script setup>
import { computed } from 'vue'
import { matClsEnum, materialPurchaseClsEnum } from '@enum-ms/classification'
import { purchaseOrderPaymentModeEnum, purchaseStatusEnum } from '@enum-ms/wms'
import { weightMeasurementModeEnum, invoiceTypeEnum, settlementStatusEnum } from '@enum-ms/finance'
import { logisticsPayerEnum, logisticsTransportTypeEnum } from '@/utils/enum/modules/logistics'
import { fileClassifyEnum } from '@enum-ms/file'
import { projectNameFormatter } from '@/utils/project'
import { isBlank, isNotBlank } from '@/utils/data-type'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'

import { regDetail } from '@compos/use-crud'
import uploadList from '@comp/file-upload/UploadList.vue'
import useMaxHeight from '@/composables/use-max-height'

import detailTable from '../components/detail-table/index.vue'

const { CRUD, crud, detail } = regDetail()

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

// 是否
const boolPurchaseCompleted = computed(() => detail.purchaseStatus === purchaseStatusEnum.FINISHED.V)
const boolSettled = computed(() => detail.settlementStatus === settlementStatusEnum.SETTLED.V)

const boolUseRequisitions = computed(() => Boolean(detail.applyPurchase?.length))

// 详情加载
CRUD.HOOK.beforeDetailLoaded = async (crud, detail) => {
  await setSpecInfoToList(detail.details)
  await numFmtByBasicClass(detail.details, {
    toNum: true
  })
  const applyPurchaseObj = {}
  if (detail.applyPurchase?.length) {
    for (const item of detail.applyPurchase) {
      applyPurchaseObj[item.id] = item
    }
  }
  detail.details.forEach((item) => {
    item.purchaseSN = applyPurchaseObj?.[item.applyPurchaseId]?.serialNumber
  })
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
    width: 450px;
    height: 100%;
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
      .right-head-content {
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

    .table-remark {
      height: 45px;
      line-height: 45px;
      display: flex;
      border: 1px solid #ebeef5;
      border-top-width: 0;
      font-size: 12px;
      color: #606266;
      .title {
        width: 60px;
        text-align: center;
      }
      .con {
        width: 200px;
        padding: 0px 10px;
        display: -webkit-box;
        overflow: hidden;
        text-overflow: ellipsis;
        -webkit-line-clamp: 2;
        -webkit-box-orient: vertical;
      }
    }
  }
}

.vertical-dashed-divider {
  margin: 0 16px 0 1px;
}
</style>
