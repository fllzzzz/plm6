<template>
  <common-drawer ref="drawerRef" title="提交预览" v-model="drawerVisible" direction="rtl" :before-close="handleClose" size="100%">
    <template #titleRight>
      <common-button :loading="loading" size="mini" type="primary" @click="submitIt"> 确认提交 </common-button>
    </template>
    <template #content>
      <div class="main-content">
        <el-form v-if="isNotBlank(form)" :model="form" size="small" label-position="left" label-width="100px">
          <div class="form-content" :style="heightStyle">
            <div class="form-left">
              <el-form-item label="物料种类" prop="basicClass">
                <div class="flex-rss child-mr-10">
                  <span v-parse-enum="{ e: baseMaterialTypeEnum, v: form.purchaseType, extra: '：' }" />
                  <span v-parse-enum="{ e: matClsEnum, v: form.basicClass, bit: true, split: ' | ' }" />
                </div>
              </el-form-item>

              <template v-if="form.supplyType === orderSupplyTypeEnum.SELF.V">
                <el-form-item label="合同量" prop="mete">
                  <span>{{ form.mete }} {{ form.meteUnit }}</span>
                  <!-- 【<span v-parse-enum="{ e: weightMeasurementModeEnum, v: form.weightMeasurementMode }" />】 -->
                </el-form-item>

                <el-form-item label="合同额" prop="amount">
                  <span>{{ form.amount }} 元</span>
                  【<span v-parse-enum="{ e: invoiceTypeEnum, v: form.invoiceType }" />
                  <span v-if="form.invoiceType !== invoiceTypeEnum.RECEIPT.V">（{{ form.taxRate }}%）</span>】
                </el-form-item>

                <!-- <el-form-item label="发票及税率" prop="invoiceType"> </el-form-item> -->
                <el-form-item label="订单类型" prop="purchaseOrderPaymentMode">
                  <span v-parse-enum="{ e: purchaseOrderPaymentModeEnum, v: form.purchaseOrderPaymentMode }" />
                </el-form-item>
              </template>

              <el-form-item prop="weightMeasurementMode" label="计量方式">
                <span v-parse-enum="{ e: weightMeasurementModeEnum, v: form.weightMeasurementMode }" />
              </el-form-item>

              <el-form-item label="物流信息" prop="logistics">
                <span v-parse-enum="{ e: logisticsTransportTypeEnum, v: form.logisticsTransportType }" />
                （费用<span v-parse-enum="{ e: logisticsPayerEnum, v: form.logisticsPayerType }" />）
              </el-form-item>

              <el-form-item label="供应商" prop="supplier">
                <span v-if="form.supplier">{{ form.supplier.name }}</span>
              </el-form-item>

              <el-form-item label="签订主体" prop="branchCompanyName" style="width: 100%">
                <span v-if="form.branchCompany">{{ form.branchCompany.name }}</span>
              </el-form-item>

              <el-form-item label="备注" prop="remark" style="width: 100%">
                <span style="word-break: break-all">{{ form.remark }}</span>
              </el-form-item>

              <el-form-item label="关联项目" class="el-form-item-4" prop="projectIds" style="width: 100%">
                <span v-if="baseMaterialTypeEnum.RAW_MATERIAL.V === form.purchaseType" class="pre-wrap">
                  {{ form.projects ? form.projects.map((v) => projectNameFormatter(v, null, false)).join(`\n`) : '' }}
                </span>
              </el-form-item>

              <el-divider><span class="title">附件</span></el-divider>
              <upload-list
                show-download
                :file-classify="fileClassifyEnum.PURCHASE_ORDER_ATT.V"
                v-model:files="form.attachments"
                :uploadable="false"
                empty-text="暂未上传采购合同附件"
              />
            </div>
            <div class="vertical-dashed-divider" />
            <div class="form-right">
              <div class="right-head flex-rbs">
                <span class="right-head-content">
                  <span class="label">采购清单</span>
                </span>
              </div>
              <!-- 清单列表 -->
              <detail-table :material-type="form.materialType" :list="form.details" :max-height="maxHeight - 150" />
            </div>
          </div>
        </el-form>
      </div>
    </template>
  </common-drawer>
</template>

<script setup>
import { defineProps, defineEmits, ref, inject } from 'vue'

import { matClsEnum } from '@enum-ms/classification'
import { orderSupplyTypeEnum, baseMaterialTypeEnum, purchaseOrderPaymentModeEnum } from '@enum-ms/wms'
import { weightMeasurementModeEnum, invoiceTypeEnum } from '@enum-ms/finance'
import { logisticsPayerEnum, logisticsTransportTypeEnum } from '@/utils/enum/modules/logistics'
import { fileClassifyEnum } from '@enum-ms/file'
import { projectNameFormatter } from '@/utils/project'
import { isNotBlank } from '@/utils/data-type'
import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import uploadList from '@comp/file-upload/UploadList.vue'
import detailTable from '../components/detail-table/index.vue'

const drawerRef = ref()
const emit = defineEmits(['update:visible', 'submit'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  loading: {
    type: Boolean,
    default: false
  }
})

const form = inject('crud')?.form

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible' })

// 高度
const { maxHeight } = useMaxHeight(
  {
    extraBox: ['.el-drawer__header'],
    wrapperBox: ['.el-drawer__body'],
    navbar: false,
    clientHRepMainH: true
  },
  drawerRef
)

function submitIt() {
  emit('submit')
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
      }
    }
  }
}

.vertical-dashed-divider {
  margin: 0 16px 0 1px;
}
</style>
