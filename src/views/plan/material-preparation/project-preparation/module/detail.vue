<template>
  <common-drawer
    ref="drawerRef"
    :visible="crud.detailVisible"
    :content-loading="!clsLoaded || crud.detailLoading"
    :before-close="crud.cancelDetail"
    :title="crud.detailTitle"
    :show-close="true"
    :size="1000"
    custom-class="purchase-order-detail"
  >
    <template #content>
      <div class="main-content">
        <el-form :model="detail" size="small" label-position="left" label-width="100px">
          <div class="form-content">
            <el-form-item label="签订主体" prop="branchCompanyName" style="width: 100%">
              <span v-if="detail.branchCompany">{{ detail.branchCompany.name }}</span>
            </el-form-item>

            <el-form-item label="采购合同编号" prop="serialNumber">
              {{ detail.serialNumber }}
            </el-form-item>

            <el-form-item label="供货类型" prop="supplyType">
              <span v-parse-enum="{ e: orderSupplyTypeEnum, v: detail.supplyType }" />
            </el-form-item>

            <el-form-item label="物料种类" prop="basicClass">
              <div class="flex-rss child-mr-10">
                <span v-parse-enum="{ e: baseMaterialTypeEnum, v: detail.purchaseType, extra: '：' }" />
                <span v-parse-enum="{ e: matClsEnum, v: detail.basicClass, bit: true, split: ' | ' }" />
              </div>
            </el-form-item>

            <el-form-item label="供应商" prop="supplier">
              <span v-if="detail.supplier">{{ detail.supplier.name }}</span>
            </el-form-item>

            <el-form-item
              v-if="detail.basicClass & matClsEnum.MATERIAL.V"
              label="辅材明细"
              prop="auxMaterialNames"
              style="width: 100%; word-break: break-all"
            >
              <span v-split="detail.auxMaterialNames" />
            </el-form-item>

            <template v-if="detail.supplyType == orderSupplyTypeEnum.SELF.V">
              <el-form-item label="合同量" prop="mete">
                <div class="input-underline flex-rss child-mr-10">
                  {{ detail.mete }}
                  {{ detail.meteUnit }}
                </div>
              </el-form-item>
              <el-form-item label="合同额" prop="amount"> {{ detail.amount }}元 </el-form-item>
              <el-form-item label="发票及税率" prop="invoiceType">
                <span v-parse-enum="{ e: invoiceTypeEnum, v: detail.invoiceType }" />
                <span v-if="detail.invoiceType !== invoiceTypeEnum.RECEIPT.V">（{{ detail.taxRate }}%）</span>
              </el-form-item>
              <el-form-item label="订单类型" prop="purchaseOrderPaymentMode">
                <span v-parse-enum="{ e: purchaseOrderPaymentModeEnum, v: detail.purchaseOrderPaymentMode }" />
              </el-form-item>
            </template>
            <el-form-item prop="weightMeasurementMode" label="计量方式">
              <span v-parse-enum="{ e: weightMeasurementModeEnum, v: detail.weightMeasurementMode }" />
            </el-form-item>
            <!-- <el-form-item label="提货方式" prop="pickUpMode">
              <span v-parse-enum="{ e: pickUpModeEnum, v: detail.pickUpMode }" />
            </el-form-item> -->
            <el-form-item label="物流信息" prop="logistics">
              <span v-parse-enum="{ e: logisticsTransportTypeEnum, v: detail.logisticsTransportType }" />
              （费用<span v-parse-enum="{ e: logisticsPayerEnum, v: detail.logisticsPayerType }" />）
            </el-form-item>

            <el-form-item label="采购状态" prop="purchaseStatus">
              <span
                :style="{ color: detail.purchaseStatus === purchaseStatusEnum.FINISHED.V ? 'green' : '' }"
                v-parse-enum="{ e: purchaseStatusEnum, v: detail.purchaseStatus }"
              />
              <span v-if="detail.boolUsed && detail.purchaseStatus === purchaseStatusEnum.UNFINISHED.V">（有入库）</span>
            </el-form-item>
            <el-form-item label="结算状态" prop="settlementStatus">
              <span
                :style="{ color: detail.settlementStatus === settlementStatusEnum.SETTLED.V ? 'green' : '' }"
                v-parse-enum="{ e: settlementStatusEnum, v: detail.settlementStatus }"
              />
            </el-form-item>

            <el-form-item label="创建人" prop="applicantName">
              <span v-empty-text>{{ detail.applicantName }}</span>
            </el-form-item>
            <el-form-item label="最后操作人" prop="lastOperatorName">
              <span v-empty-text>{{ detail.lastOperatorName }}</span>
            </el-form-item>

            <el-form-item label="创建时间" prop="applicantName">
              <span v-parse-time="detail.createTime" />
            </el-form-item>
            <el-form-item label="编辑时间" prop="applicantName">
              <span v-parse-time="detail.userUpdateTime" />
            </el-form-item>

            <el-form-item label="备注" prop="remark">
              <span style="word-break: break-all">{{ detail.remark }}</span>
            </el-form-item>
            <el-form-item label="申购单号" prop="requisitionsSN">
              <span class="pre-wrap">{{ detail.requisitionsSN ? detail.requisitionsSN.join(`\n`) : '' }}</span>
            </el-form-item>
            <el-form-item label="关联项目" class="el-form-item-4" prop="projectIds" style="width: 100%">
              <span v-if="baseMaterialTypeEnum.RAW_MATERIAL.V === detail.purchaseType" class="pre-wrap">
                {{ detail.projects ? detail.projects.map((v) => projectNameFormatter(v, null, false)).join(`\n`) : '' }}
              </span>
            </el-form-item>
            <area-table-tree
              v-if="baseMaterialTypeEnum.MANUFACTURED.V === detail.purchaseType && detail.basicClass && isNotBlank(detail.projectIds)"
              v-model:struc="detail.strucAreaIds"
              v-model:encl="detail.enclAreaIds"
              :show-struc="!!(detail.basicClass & matClsEnum.STRUC_MANUFACTURED.V)"
              :show-encl="!!(detail.basicClass & matClsEnum.ENCL_MANUFACTURED.V)"
              :project-ids="detail.projectIds"
              only-show-checked
              style="width: 100%"
            />

            <el-divider><span class="title">附件</span></el-divider>
            <upload-list
              show-download
              :file-classify="fileClassifyEnum.PURCHASE_ORDER_ATT.V"
              v-model:files="detail.attachments"
              :uploadable="false"
              empty-text="暂未上传采购合同编号附件"
            />
          </div>
        </el-form>
      </div>
    </template>
  </common-drawer>
</template>

<script setup>
import { watch, nextTick } from 'vue'
import { matClsEnum } from '@enum-ms/classification'
import { orderSupplyTypeEnum, baseMaterialTypeEnum, purchaseOrderPaymentModeEnum, purchaseStatusEnum } from '@enum-ms/wms'
import { weightMeasurementModeEnum, invoiceTypeEnum, settlementStatusEnum } from '@enum-ms/finance'
import { logisticsPayerEnum, logisticsTransportTypeEnum } from '@/utils/enum/modules/logistics'
import { fileClassifyEnum } from '@enum-ms/file'
import { projectNameFormatter } from '@/utils/project'
import { isNotBlank } from '@/utils/data-type'

import { regDetail } from '@compos/use-crud'
import uploadList from '@comp/file-upload/UploadList.vue'
import areaTableTree from '@/components-system/branch-sub-items/outsourcing-area-table-tree.vue'
import useMatClsList from '@/composables/store/use-mat-class-list'

const { CRUD, crud, detail } = regDetail()

const { loaded: clsLoaded, rawMatClsKV } = useMatClsList()

CRUD.HOOK.beforeDetailLoaded = (crud, detail) => {
  const setInfo = () => {
    if (detail.auxMaterialIds) {
      detail.auxMaterialNames = detail.auxMaterialIds.map((id) => {
        const material = rawMatClsKV.value[id]
        if (material) {
          return material.name
        }
        return '-'
      })
    }
  }
  if (!clsLoaded.value) {
    const trigger = watch(
      clsLoaded,
      (val) => {
        if (val) {
          setInfo()
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
</script>

<style lang="scss" scoped>
.form-content {
  display: flex;
  flex-wrap: wrap;
  ::v-deep(.el-form-item) {
    width: 450px;
  }
  ::v-deep(.el-form-item__content) {
    padding-right: 20px;
  }
}
</style>
