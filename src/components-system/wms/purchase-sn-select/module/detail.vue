<template>
  <common-drawer
    ref="drawerRef"
    :visible="visible"
    :content-loading="loading"
    :before-close="handleClose"
    title="采购订单详情"
    :show-close="true"
    :size="1000"
    custom-class="purchase-order-detail"
  >
    <template #content>
      <div class="main-content">
        <el-form :model="detail" size="small" label-position="left" label-width="100px">
          <div class="form-content">
            <el-form-item label="采购订单" prop="serialNumber">
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

            <el-form-item label="供应商" prop="supplierId">
              {{ detail.supplier ? detail.supplier.name : '' }}
            </el-form-item>

            <template v-if="detail.supplyType == orderSupplyTypeEnum.SELF.V">
              <el-form-item label="合同量" prop="mete">
                <div class="input-underline flex-rss child-mr-10">
                  {{ detail.mete }}
                  {{ detail.meteUnit }}
                </div>
              </el-form-item>
              <el-form-item label="订单类型" prop="purchaseOrderPaymentMode">
                <span v-parse-enum="{ e: purchaseOrderPaymentModeEnum, v: detail.purchaseOrderPaymentMode }" />
              </el-form-item>
            </template>
            <el-form-item prop="weightMeasurementMode" label="计量方式">
              <span v-parse-enum="{ e: weightMeasurementModeEnum, v: detail.weightMeasurementMode }" />
            </el-form-item>
            <el-form-item label="提货方式" prop="pickUpMode">
              <span v-parse-enum="{ e: pickUpModeEnum, v: detail.pickUpMode }" />
            </el-form-item>
            <el-form-item label="备注" prop="remark">
              <span>{{ detail.remark }}</span>
            </el-form-item>

            <el-form-item label="申购单号" prop="requisitionsSN">
              <span class="pre-wrap">{{ detail.requisitionsSN ? detail.requisitionsSN.join(`\n`) : '' }}</span>
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

            <el-form-item label="创建人" prop="founderName">
              <span v-empty-text>{{ detail.founderName }}</span>
            </el-form-item>
            <el-form-item label="最后操作人" prop="lastOperatorName">
              <span v-empty-text>{{ detail.lastOperatorName }}</span>
            </el-form-item>

            <el-form-item label="关联项目" class="el-form-item-4" prop="projectIds" style="width: 900px">
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
              v-model:files="detail.attachment"
              :uploadable="false"
              empty-text="暂未上传采购订单附件"
            />
          </div>
        </el-form>
      </div>
    </template>
  </common-drawer>
</template>

<script setup>
import { detail as detailApi } from '@/api/wms/purchase-order'
import { defineEmits, defineProps, ref, watch } from 'vue'
import { matClsEnum } from '@enum-ms/classification'
import { orderSupplyTypeEnum, baseMaterialTypeEnum, pickUpModeEnum, purchaseOrderPaymentModeEnum, purchaseStatusEnum } from '@enum-ms/wms'
import { weightMeasurementModeEnum, settlementStatusEnum } from '@enum-ms/finance'
import { fileClassifyEnum } from '@enum-ms/file'
import { projectNameFormatter } from '@/utils/project'
import { isNotBlank } from '@/utils/data-type'

import uploadList from '@comp/file-upload/UploadList.vue'
import areaTableTree from '@/components-system/branch-sub-items/outsourcing-area-table-tree.vue'
import useVisible from '@/composables/use-visible'

const emit = defineEmits(['success', 'update:modelValue'])

const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  },
  detailId: {
    type: Number
  }
})

const detail = ref({})
const loading = ref(false)
const { visible, handleClose } = useVisible({ emit, props })

watch(
  () => props.detailId,
  (val) => {
    if (val) {
      getDetail(val)
    }
  },
  { immediate: true }
)

// 加载详情
async function getDetail(id) {
  loading.value = true
  detail.value = {}
  try {
    detail.value = await detailApi(id)
  } catch (error) {
    console.log('采购单详情', error)
  } finally {
    loading.value = false
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
  ::v-deep(.el-form-item:nth-child(even)) {
    margin-left: 20px;
  }
}
</style>
