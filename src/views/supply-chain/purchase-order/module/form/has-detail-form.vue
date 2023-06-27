<template>
  <div>
      <div style="display:flex;">
        <div>
          <slot name="chose"></slot>
        </div>
        <div style="flex:1;text-align:right;">
          <common-button :loading="cu.status.edit === FORM.STATUS.PROCESSING" size="mini" type="primary" @click="submit">
            提 交
          </common-button>
          <store-operation v-if="!props.edit" type="cu" @clear="handleClear" />
        </div>
      </div>
      <div class="main-content">
        <el-form ref="detailFormRef" :model="form" :rules="rules" size="small" label-position="right" label-width="110px">
          <div class="form-content" :style="`height: ${maxHeight.value}px`">
            <div class="form-left" :style="isFold ? 'width:0px;padding:0;' : ''">
              <div class="order-details">
                <el-form-item label="是否绑定申购" prop="useRequisitions" required>
                  <common-radio v-model="form.useRequisitions" :disabled="Boolean(form.boolUsed)" :options="whetherEnum.ENUM" type="enum" @change="clearList"/>
                </el-form-item>
                <el-form-item class="el-form-item-1" label="采购合同编号" prop="serialNumber">
                  <el-input
                    v-model.trim="form.serialNumber"
                    placeholder="可输入采购合同编号（不填写则自动生成）"
                    :disabled="Boolean(form.boolUsed)"
                    maxlength="30"
                    size="small"
                    class="input-underline"
                  />
                </el-form-item>
                <el-form-item class="el-form-item-3" label="材料类型" prop="materialType">
                  <common-radio
                    v-model="form.materialType"
                    :disabled="Boolean(form.boolUsed)"
                    :disabledVal="[materialPurchaseClsEnum.MANUFACTURED.V]"
                    :options="materialPurchaseClsEnum.ENUM"
                    type="enum"
                  />
                </el-form-item>
                <el-form-item
                  v-if="form.materialType & materialPurchaseClsEnum.MATERIAL.V && !form.useRequisitions"
                  class="el-form-item-5"
                  label="辅材明细"
                  prop="auxMaterialIds"
                >
                  <div class="flex-rss child-mr-10">
                    <!-- 是否选择所有辅材 -->
                    <el-checkbox
                      v-model="form.isAllMaterial"
                      :disabled="form.boolUsed"
                      label="所有辅材"
                      size="mini"
                      border
                      style="margin-top: 3px; margin-right: 5px"
                    />
                    <material-cascader
                      v-model="form.auxMaterialIds"
                      :basic-class="matClsEnum.MATERIAL.V"
                      :deep="2"
                      :disabled="form.boolUsed || form.isAllMaterial"
                      multiple
                      :collapse-tags="false"
                      separator=" > "
                      clearable
                      placeholder="请选择辅材"
                      class="input-underline"
                      size="small"
                      style="width: 100%"
                    />
                  </div>
                </el-form-item>
                <el-form-item
                  v-if="form.materialType & materialPurchaseClsEnum.OTHER.V && !form.useRequisitions"
                  class="el-form-item-5"
                  label="其它明细"
                  prop="otherMaterialIds"
                >
                  <div class="flex-rss child-mr-10">
                    <!-- 是否选择所有辅材 -->
                    <el-checkbox
                      v-model="form.isAllOtherMaterial"
                      :disabled="form.boolUsed"
                      label="所有其它科目"
                      size="mini"
                      border
                      style="margin-top: 3px; margin-right: 5px"
                    />
                    <material-cascader
                      v-model="form.otherMaterialIds"
                      :basic-class="matClsEnum.OTHER.V"
                      :deep="2"
                      :disabled="form.boolUsed || form.isAllOtherMaterial"
                      multiple
                      :collapse-tags="false"
                      separator=" > "
                      clearable
                      placeholder="请选择其它科目"
                      class="input-underline"
                      size="small"
                      style="width: 100%"
                    />
                  </div>
                </el-form-item>
                <el-form-item v-if="!form.useRequisitions && isManuf" label="选择项目" class="el-form-item-4" prop="projectId">
                  <project-cascader v-model="form.projectId" clearable :disabled="Boolean(form.boolUsed)" class="input-underline" />
                </el-form-item>
                <el-form-item class="el-form-item-7" label="供应商" prop="supplierId">
                  <supplier-select
                    v-model="form.supplierId"
                    :basic-class="form.basicClass"
                    :type="form.purchaseType"
                    :disabled="Boolean(form.boolUsed)"
                    mode="cross"
                    clearable
                    filterable
                    class="input-underline"
                  />
                </el-form-item>
                <el-form-item class="el-form-item-18" label="签订主体" prop="branchCompanyId">
                  <branch-company-select
                    v-model="form.branchCompanyId"
                    class="input-underline"
                    placeholder="合同签订主体"
                    :disabled="Boolean(form.boolUsed)"
                    default
                    style="width: 100%"
                  />
                </el-form-item>
                <el-form-item class="el-form-item-10" label="发票及税率" prop="invoiceType">
                  <invoice-type-select
                    class="input-underline"
                    v-model:invoiceType="form.invoiceType"
                    v-model:taxRate="form.taxRate"
                    :disabled="Boolean(form.boolUsed)"
                    :classification="
                      form.materialType & materialPurchaseClsEnum.OTHER.V ? materialPurchaseClsEnum.MATERIAL.V : form.materialType
                    "
                  />
                </el-form-item>
                <el-form-item class="el-form-item-13" label="订单类型" prop="purchaseOrderPaymentMode">
                  <common-radio
                    v-model="form.purchaseOrderPaymentMode"
                    :options="purchaseOrderPaymentModeEnum.ENUM"
                    type="enum"
                    :disabled="Boolean(form.boolUsed)"
                    :size="'small'"
                  />
                </el-form-item>
                <el-form-item class="el-form-item-16" label="物流运输方式" prop="logisticsTransportType">
                  <common-radio
                    v-model="form.logisticsTransportType"
                    :options="logisticsTransportTypeEnum.ENUM"
                    type="enum"
                    :disabled="Boolean(form.boolUsed)"
                    size="small"
                  />
                </el-form-item>
                <el-form-item class="el-form-item-17" label="物流费用承担" prop="logisticsPayerType">
                  <common-radio
                    v-model="form.logisticsPayerType"
                    :options="logisticsPayerEnum.ENUM"
                    type="enum"
                    :disabled="Boolean(form.boolUsed)"
                    size="small"
                  />
                </el-form-item>
                <el-form-item
                  v-if="!form.useRequisitions && !isManuf"
                  label="选择项目"
                  class="el-form-item-4"
                  prop="projectIds"
                  label-width="80px"
                >
                  <project-cascader
                    v-model="form.projectIds"
                    clearable
                    :disabled="Boolean(form.boolUsed)"
                    multiple
                    class="input-underline"
                  />
                </el-form-item>
                <el-form-item class="el-form-item-14" prop="remark" label-width="0">
                  <el-input v-model="form.remark" :rows="3" type="textarea" placeholder="备注" maxlength="1000" show-word-limit />
                </el-form-item>
                <upload-list
                  class="el-form-item-15"
                  show-download
                  :file-classify="fileClassifyEnum.PURCHASE_ORDER_ATT.V"
                  v-model:files="form.attachments"
                  empty-text="暂未上传采购合同附件"
                />
              </div>
            </div>
            <div class="vertical-dashed-divider" :style="isFold ? 'display:none;' : ''" />
            <div class="form-right">
              <hamburger :is-active="isFold" class="hamburger-container" @toggleClick="isFold = !isFold" />
              <div class="right-head flex-rbs" v-if="form.useRequisitions">
                <!-- 关联申购单-->
                <span class="right-head-content">
                  <span class="label">关联申购单号</span>
                  <common-radio-button
                    v-if="form.materialType & materialPurchaseClsEnum.STEEL.V && !form.boolUsed"
                    type="enum"
                    v-model="form.currentBasicClass"
                    :options="steelClsEnum.ENUM"
                    :disabledVal="[steelClsEnum.STEEL_COIL.V]"
                    clearable
                    style="vertical-align: middle; margin-right: 6px"
                  >
                    <template #suffix="{ item }">
                      <span v-if="form[compListVK[item.V]]?.length">({{ form[compListVK[item.V]]?.length }})</span>
                    </template>
                  </common-radio-button>
                  <el-tag v-for="item in form.actualRequisitionIds" :key="item" effect="plain" class="preparation-sn-tag">
                    {{ form.requisitionsKV?.[item]?.serialNumber }}
                  </el-tag>
                </span>
                <span class="opt-content" v-if="!form.boolUsed">
                  <common-button type="success" size="mini" @click="addRequisition"> 选择申购物料 </common-button>
                </span>
              </div>
              <div class="right-head flex-rbs" v-if="!form.useRequisitions">
                <!-- 自建采购清单 -->
                <span class="right-head-content">
                  <span class="label">采购清单</span>
                  <common-radio-button
                    v-if="form.materialType & materialPurchaseClsEnum.STEEL.V && !form.boolUsed"
                    type="enum"
                    v-model="form.currentBasicClass"
                    :options="steelClsEnum.ENUM"
                    :disabledVal="[]"
                    clearable
                    style="vertical-align: middle"
                  >
                    <template #suffix="{ item }">
                      <span v-if="form[compListVK[item.V]]?.length">({{ form[compListVK[item.V]]?.length }})</span>
                    </template>
                  </common-radio-button>
                </span>
                <span class="opt-content" v-if="!form.boolUsed">
                  <common-button
                    v-if="!isManuf && !form.boolUsed"
                    class="filter-item"
                    type="success"
                    size="mini"
                    @click="addMaterialSelf"
                  >
                    添加物料
                  </common-button>
                  <excel-resolve-button
                    v-if="!isManuf"
                    icon="el-icon-upload2"
                    btn-name="清单导入"
                    btn-size="mini"
                    class="filter-item"
                    btn-type="warning"
                    open-loading
                    style="margin-left: 10px"
                    :template="importTemp"
                    @success="handleExcelSuccess"
                  />
                  <export-button
                    v-if="!isManuf"
                    class="filter-item"
                    style="margin-left: 10px"
                    type="info"
                    :params="{ basicClass: form.currentBasicClass, receiptType: receiptTypeEnum.PURCHASE.V }"
                    :fn="downloadExcelTemplate"
                  >
                    {{ matClsEnum.VL[form.currentBasicClass] }}清单模板下载
                  </export-button>
                  <el-tooltip :disabled="!!form.projectId" effect="light" content="请先选择项目" placement="left-start">
                    <span>
                      <common-button
                        v-if="isManuf"
                        class="filter-item"
                        size="mini"
                        type="success"
                        icon="el-icon-plus"
                        @click="purchaseManufVisible = true"
                        :disabled="!form.projectId"
                      >
                        添加制成品
                      </common-button>
                    </span>
                  </el-tooltip>
                </span>
              </div>
              <!-- 清单列表 -->
              <component v-if="!form.boolUsed" ref="compRef" :is="currentView" :maxHeight="maxHeight - 150" />
              <detail-table
                v-else
                :material-type="form.materialType"
                :list="form.details"
                :max-height="maxHeight - 150"
                :bool-use-requisitions="form.useRequisitions"
              />
              <div class="table-remark">
                <template v-if="!Boolean(form.materialType & (materialPurchaseClsEnum.MATERIAL.V | materialPurchaseClsEnum.OTHER.V))">
                  <span class="title">合同量</span>
                  <span class="con">
                    <span>{{ form.mete }}</span>
                    <span>{{ form.meteUnit }}</span>
                  </span>
                </template>
                <span class="title">合同额</span>
                <span class="con">{{ form.amount }} 元</span>
              </div>
            </div>
          </div>
        </el-form>
      </div>
      <common-drawer v-model="requisitionVisible" title="申购选择" direction="btt" size="70%" custom-class="material-requisition-select">
        <template #titleRight>
          <!-- <el-tag v-if="requisitionBadge" type="success" effect="plain">已加入（{{ requisitionBadge }}）</el-tag> -->
        </template>
        <template #content>
          <requisition-list-application ref="requisitionListRef" @add-purchase="handleAddPurchase" />
        </template>
      </common-drawer>
      <common-drawer
        ref="materialSpecSelectDrawer"
        v-model="materialSelectVisible"
        title="物料选择"
        :show-close="true"
        :size="900"
        custom-class="material-table-spec-select"
      >
        <template #content>
          <material-table-spec-select
            ref="matSpecRef"
            v-model="form[compListVK[form.currentBasicClass]]"
            :visible="materialSelectVisible"
            :row-init-fn="rowInit"
            :max-height="specSelectMaxHeight"
            :basic-class="form.currentBasicClass"
            :classify-ids="
              form.materialType & materialPurchaseClsEnum.MATERIAL.V
                ? form.auxMaterialIds
                : form.materialType & materialPurchaseClsEnum.OTHER.V
                ? form.otherMaterialIds
                : []
            "
            :table-width="350"
            auto-selected
            expand-query
          />
        </template>
      </common-drawer>
      <common-drawer
        ref="manufSelectDrawerRef"
        show-close
        size="80%"
        title="制成品选择"
        v-model="purchaseManufVisible"
        custom-class="manufactured-select-drawer"
      >
        <template #content>
          <manuf-list
            :project-id="form.projectId"
            :visible="purchaseManufVisible"
            :maxHeight="manufSelectMaxHeight"
            @add="handleAddManuf"
          />
        </template>
      </common-drawer>
    </div>
</template>

<script setup>
import { add, edit } from '@/api/supply-chain/purchase-order'
import { downloadExcelTemplate } from '@/api/wms/common'
import { ref, computed, provide, nextTick, watchEffect, watch, defineProps, defineEmits } from 'vue'
import { matClsEnum, steelClsEnum, materialPurchaseClsEnum } from '@enum-ms/classification'
import { baseMaterialTypeEnum, purchaseOrderPaymentModeEnum, receiptTypeEnum } from '@enum-ms/wms'
import { logisticsPayerEnum, logisticsTransportTypeEnum } from '@/utils/enum/modules/logistics'
import { invoiceTypeEnum } from '@enum-ms/finance'
import { fileClassifyEnum } from '@enum-ms/file'
import { whetherEnum } from '@enum-ms/common'
import { isBlank, toPrecision, deepClone } from '@/utils/data-type'
import { clearObject } from '@/utils/data-type/object'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { obj2arr } from '@/utils/convert/type'
import { uniqueArr } from '@/utils/data-type/array'
import steelPlateTemp from '@/utils/excel/import-template/supply-chain/purchase-temp/steel-plate'
import sectionSteelTemp from '@/utils/excel/import-template/supply-chain/purchase-temp/section-steel'
import steelCoilTemp from '@/utils/excel/import-template/supply-chain/purchase-temp/steel-coil'
import auxMaterialTemp from '@/utils/excel/import-template/supply-chain/purchase-temp/aux-material'

// import { regForm } from '@compos/use-crud'
import useForm from '@/composables/form/use-form'
// import UnitSelect from '@comp-common/unit-select/index.vue'
import ProjectCascader from '@comp-base/project-cascader.vue'
import SupplierSelect from '@comp-base/supplier-select/index.vue'
import BranchCompanySelect from '@comp-base/branch-company-select.vue'
import MaterialCascader from '@comp-cls/material-cascader/index.vue'
import InvoiceTypeSelect from '@/components-system/base/invoice-type-select.vue'
import UploadList from '@comp/file-upload/UploadList.vue'
import StoreOperation from '@crud/STORE.operation.vue'
import useMaxHeight from '@/composables/use-max-height'

import detailTable from '../components/detail-table/index.vue'
import RequisitionListApplication from '../components/requisition-list/index.vue'
import ManufList from '../components/manuf-list.vue'
// import SubmitPreview from '../components/submit-preview.vue'
import SteelApplication from '../components/application/steel/index'
import AuxMatApplication from '../components/application/auxiliary-material/index'
import OtherApplication from '../components/application/other/index'
import ManufApplication from '../components/application/manufactured/index'
import materialTableSpecSelect from '@/components-system/classification/material-table-spec-select.vue'
import excelResolveButton from '@/components-system/common/excel-resolve-button/index.vue'
import ExportButton from '@comp-common/export-button/index.vue'
import Hamburger from '@comp/Hamburger/index.vue'
import { ElMessage } from 'element-plus'

const emit = defineEmits('success')

const defaultForm = {
  attachmentIds: undefined, // 附件ids
  list: [],
  sectionSteelList: [],
  steelPlateList: [],
  steelCoilList: [],
  requisitions: [],
  requisitionsKV: {},
  actualRequisitionIds: [],
  manufListObj: {},
  manufMergeObj: {}
}

const props = defineProps({
  edit: {
    type: Boolean,
    default: false
  },
  dialogVisible: {
    type: Boolean,
    default: false
  },
  detail: {
    type: Object
  },
  maxHeight: {
    type: Number,
    default: 100
  }
})

// 是否展开
const isFold = ref(false)

const detailFormRef = ref() // 表单

const { cu, form, FORM } = useForm(
  {
    title: '采购合同',
    formStore: !props.edit,
    formStoreKey: 'SUPPLY_CHAIN_PURCHASE_ORDER',
    defaultForm: defaultForm,
    useDraftCallback: setFormCallback,
    clearDraftCallback: init,
    api: props.edit ? edit : add
  },
  detailFormRef,
  props.detail
)

watch(
  () => props.detail,
  (val = {}) => {
    form.useRequisitions = val.useRequisitions// 是否绑定申购单
    form.materialType = props.detail.materialType // 材料类型
    form.purchaseType = props.detail.purchaseType// 物料种类
    form.currentBasicClass = props.detail.currentBasicClass // 物料类型
    form.supplyType = props.detail.supplyType // 供货类型
    form.boolUsed = props.detail.boolUsed
  },
  { deep: true, immediate: true }
)

provide('cu', cu)

function setFormCallback() {

}

function init() {

}

// ------------------------- rules start -----------------------------------
const validateInvoiceType = (rule, value, callback) => {
  if (form.invoiceType || form.invoiceType === 0) {
    if (form.invoiceType === invoiceTypeEnum.SPECIAL.V && !form.taxRate) {
      callback(new Error('请选择税率'))
      return
    } else {
      callback()
    }
  } else {
    callback(new Error('请选择发票及税率'))
    return
  }
}

// 校验规则
const rules = ref({})

// 基础校验
const baseRules = {
  serialNumber: [{ max: 30, message: '订单编号长度不可超过30位', trigger: 'blur' }],
  materialType: [{ required: true, message: '请选择材料类型', trigger: 'change' }],
  projectId: [{ required: true, message: '请选择项目', trigger: 'change' }],
  supplyType: [{ required: true, message: '请选择供货类型', trigger: 'change' }],
  supplierId: [{ required: true, message: '请选择供应商', trigger: 'change' }],
  branchCompanyId: [{ required: true, message: '请选择签订主体', trigger: 'change' }]
}

// 自采物料校验
const selfRules = {
  // weightMeasurementMode: [{ required: true, message: '请选择计量方式', trigger: 'change' }],
  logisticsTransportType: [{ required: true, message: '请选择物流运输方式', trigger: 'change' }],
  logisticsPayerType: [{ required: true, message: '请选择物流费用承担方', trigger: 'change' }],
  purchaseOrderPaymentMode: [{ required: true, message: '请选择订单类型', trigger: 'change' }],
  invoiceType: [{ required: true, validator: validateInvoiceType, trigger: 'change' }],
  taxRate: [{ max: 2, message: '请输入税率', trigger: 'blur' }],
  mete: [{ required: true, message: '请填写合同量', trigger: 'blur' }],
  amount: [{ required: true, message: '请填写合同额', trigger: 'blur' }]
}

const validateAuxMat = (rule, value, callback) => {
  if (!form.isAllMaterial) {
    if (!value) {
      callback(new Error('请选择辅材'))
      return
    } else {
      callback()
    }
  } else {
    callback()
  }
}

// 辅材校验
const auxMatRules = {
  auxMaterialIds: [{ required: true, validator: validateAuxMat, trigger: 'change' }]
}

const validateOtherMat = (rule, value, callback) => {
  if (!form.isAllOtherMaterial) {
    if (!value) {
      callback(new Error('请选择其它科目'))
      return
    } else {
      callback()
    }
  } else {
    callback()
  }
}

// 其它校验
const otherMatRules = {
  otherMaterialIds: [{ required: true, validator: validateOtherMat, trigger: 'change' }]
}

// rules变更
watchEffect(() => {
  if (props.dialogVisible) {
    const r = rules.value
    clearObject(r)
    Object.assign(r, baseRules)
    Object.assign(r, selfRules)
    if (form.materialType & materialPurchaseClsEnum.MATERIAL.V && !form.useRequisitions) {
      Object.assign(r, auxMatRules)
    }
    if (form.materialType & materialPurchaseClsEnum.OTHER.V && !form.useRequisitions) {
      Object.assign(r, otherMatRules)
    }
    nextTick(() => {
      detailFormRef.value && detailFormRef.value.clearValidate()
    })
  }
})
// ------------------------- rules end -------------------------------------

// 是否制成品
const isManuf = computed(() => Boolean(form.materialType & materialPurchaseClsEnum.MANUFACTURED.V))

watch(
  () => form.materialType,
  (val) => {
    if (val & materialPurchaseClsEnum.MATERIAL.V) {
      form.basicClass = form.currentBasicClass = matClsEnum.MATERIAL.V
    }
    if (val & materialPurchaseClsEnum.OTHER.V) {
      form.basicClass = form.currentBasicClass = matClsEnum.OTHER.V
    }
    if (val & materialPurchaseClsEnum.STEEL.V) {
      form.basicClass = matClsEnum.STEEL_PLATE.V | matClsEnum.STEEL_COIL.V | matClsEnum.SECTION_STEEL.V
      form.currentBasicClass = matClsEnum.STEEL_PLATE.V
    }
    if (val & materialPurchaseClsEnum.MANUFACTURED.V) {
      form.basicClass = form.currentBasicClass = matClsEnum.STRUC_MANUFACTURED.V
      form.purchaseType = baseMaterialTypeEnum.MANUFACTURED.V
    } else {
      form.purchaseType = baseMaterialTypeEnum.RAW_MATERIAL.V
    }
    clearList()
    if (matSpecRef.value) {
      matSpecRef.value.clear()
    }
  }
)

// 监听useRequisitions变化，清空列表
// watch(
//   () => form.useRequisitions,
//   (val) => {
//     clearList()
//   }
// )

const currentView = computed(() => {
  switch (form.materialType) {
    case materialPurchaseClsEnum.STEEL.V:
      return SteelApplication
    case materialPurchaseClsEnum.MATERIAL.V:
      return AuxMatApplication
    case materialPurchaseClsEnum.OTHER.V:
      return OtherApplication
    case materialPurchaseClsEnum.MANUFACTURED.V:
      return ManufApplication
    default:
      return SteelApplication
  }
})

// 清空列表
const clearList = () => {
  form.list = []
  form.sectionSteelList = []
  form.steelPlateList = []
  form.steelCoilList = []
  form.requisitionsKV = {}
  form.actualRequisitionIds = []
  form.manufListObj = {}
  // form.manufMergeObj = {}
}

// --------------------------- 自选采购 start ------------------------------

const compListVK = {
  [matClsEnum.STEEL_PLATE.V]: 'steelPlateList',
  [matClsEnum.SECTION_STEEL.V]: 'sectionSteelList',
  [matClsEnum.STEEL_COIL.V]: 'steelCoilList',
  [matClsEnum.MATERIAL.V]: 'list',
  [matClsEnum.OTHER.V]: 'list',
  [matClsEnum.STRUC_MANUFACTURED.V]: 'list',
  [matClsEnum.ENCL_MANUFACTURED.V]: 'list'
}

// 当前物料“批量导入模板”
const importTemp = computed(() => {
  switch (form.currentBasicClass) {
    case matClsEnum.STEEL_PLATE.V:
      return steelPlateTemp
    case matClsEnum.SECTION_STEEL.V:
      return sectionSteelTemp
    case matClsEnum.STEEL_COIL.V:
      return steelCoilTemp
    case matClsEnum.MATERIAL.V:
      return auxMaterialTemp
    default:
      return auxMaterialTemp
  }
})

const compRef = ref()
const materialSpecSelectDrawer = ref()
const manufSelectDrawerRef = ref()
const matSpecRef = ref()
const materialSelectVisible = ref(false)
const purchaseManufVisible = ref(false)

provide('matSpecRef', matSpecRef) // 供兄弟组件调用 删除

// 物料选择组件高度
const { maxHeight: specSelectMaxHeight } = useMaxHeight(
  {
    mainBox: '.material-table-spec-select',
    extraBox: '.el-drawer__header',
    wrapperBox: ['.el-drawer__body'],
    navbar: false,
    clientHRepMainH: true,
    minHeight: 300
  },
  () => materialSpecSelectDrawer.value?.loaded
)

// 制成品选择组件高度
const { maxHeight: manufSelectMaxHeight } = useMaxHeight(
  {
    mainBox: '.manufactured-select-drawer',
    extraBox: ['.el-drawer__header', '.head-container'],
    wrapperBox: ['.el-drawer__body'],
    navbar: false,
    paginate: true,
    clientHRepMainH: true,
    extraHeight: 50,
    minHeight: 300
  },
  () => manufSelectDrawerRef.value?.loaded
)

// 行数据添加时初始化
function rowInit(row) {
  return compRef.value.rowInit(row)
}

// --------------------------- 自选采购 end --------------------------------
function addMaterialSelf() {
  if (form.materialType & materialPurchaseClsEnum.MATERIAL.V && (!form.isAllMaterial && !form.auxMaterialIds?.length)) {
    ElMessage.warning('请先选择辅材明细')
    return
  }
  if (form.materialType & materialPurchaseClsEnum.OTHER.V && (!form.isAllOtherMaterial && !form.otherMaterialIds?.length)) {
    ElMessage.warning('请先选择其它明细')
    return
  }
  materialSelectVisible.value = true
}

// 添加制成品
function handleAddManuf(list) {
  for (let i = 0; i < list.length; i++) {
    const v = deepClone(list[i])
    // v.measureUnit = '件' // 计量单位
    // v.accountingUnit = '千克' // 核算单位
    const _purchaseWeight = v.curPurchaseQuantity * v.netWeight || 0
    if (isBlank(form.manufListObj[v.id])) {
      form.manufListObj[v.id] = {
        ...v,
        rowKey: v.id,
        curPurchaseWeight: _purchaseWeight
      }
    } else {
      form.manufListObj[v.id].curPurchaseQuantity += v.curPurchaseQuantity
      form.manufListObj[v.id].curPurchaseWeight = toPrecision(form.manufListObj[v.id].curPurchaseWeight + _purchaseWeight, 2)
    }
    // const _mergeStr = v.name ? v.name + '_' + v.specification + '_' + v.material : v.specification + '_' + v.material
    // if (isBlank(form.manufMergeObj[_mergeStr])) {
    //   form.manufMergeObj[_mergeStr] = {
    //     ...v,
    //     rowKey: _mergeStr,
    //     mergeIds: [v.id],
    //     curPurchaseQuantity: v.curPurchaseQuantity,
    //     curPurchaseWeight: _purchaseWeight
    //   }
    // } else {
    //   form.manufMergeObj[_mergeStr].mergeIds.push(v.id)
    //   form.manufMergeObj[_mergeStr].curPurchaseQuantity += v.curPurchaseQuantity
    //   form.manufMergeObj[_mergeStr].curPurchaseWeight = toPrecision(form.manufMergeObj[_mergeStr].curPurchaseWeight + _purchaseWeight, 2)
    // }
  }
  purchaseManufVisible.value = false
}

// 解析导入表格
function handleExcelSuccess(importList) {
  // 解析
  // 根据物料种类获取
  try {
    const key = compListVK[form.currentBasicClass]
    // 截取新旧数组长度，对导入数据进行rowWatch监听
    const oldLen = form[key].length
    form[key].push.apply(form[key], importList)
    const newLen = form[key].length
    if (compRef.value?.rowWatch) {
      for (let i = oldLen; i < newLen; i++) {
        compRef.value?.rowWatch(form[key][i])
      }
    }
    // 初始化选中数据，执行一次后取消当前监听
    const initSelectedTrigger = watch(
      matSpecRef,
      () => {
        if (matSpecRef.value) {
          matSpecRef.value.initSelected(
            importList.map((v) => {
              return { sn: v.sn, classifyId: v.classifyId }
            })
          )
          nextTick(() => {
            initSelectedTrigger()
          })
        }
      },
      { immediate: true }
    )
  } catch (error) {
    ElMessage.error({ message: error.message, duration: 5000 })
  }
}

// --------------------------- 申购 start ------------------------------

const requisitionListRef = ref()
const requisitionVisible = ref(false)

// const requisitionBadge = computed(() => (form.requisitionListKV ? Object.keys(form.requisitionListKV)?.length : 0))

watchEffect(() => {
  form.requisitions = obj2arr(form.requisitionsKV)
  const _actualRequisitionIds = []
  if (form.materialType & materialPurchaseClsEnum.STEEL.V) {
    form.steelPlateList?.forEach((v) => {
      _actualRequisitionIds.push(v.applyPurchaseId)
    })
    form.steelCoilList?.forEach((v) => {
      _actualRequisitionIds.push(v.applyPurchaseId)
    })
    form.sectionSteelList?.forEach((v) => {
      _actualRequisitionIds.push(v.applyPurchaseId)
    })
  }
  if (form.materialType & (materialPurchaseClsEnum.MATERIAL.V | materialPurchaseClsEnum.OTHER.V)) {
    form.list?.forEach((v) => {
      _actualRequisitionIds.push(v.applyPurchaseId)
    })
  }
  form.actualRequisitionIds = uniqueArr(_actualRequisitionIds)
})

function addRequisition() {
  requisitionVisible.value = true
  nextTick(() => requisitionListRef.value?.openInit())
}

function handleAddPurchase(row) {
  // 制成品 row 为数组
  if (form.materialType & materialPurchaseClsEnum.MANUFACTURED.V) {
    handleAddManuf(row)
  } else {
    form[compListVK[row.basicClass]].push(row)
    if (row.basicClass & form.currentBasicClass) {
      compRef.value?.rowWatch(row)
    }
  }
}

// --------------------------- 申购 end --------------------------------

function handleClear() {
  if (matSpecRef.value) {
    matSpecRef.value.clear()
  }
}

FORM.HOOK.beforeToCU = () => {
  let _trigger
  if (!(form.materialType & materialPurchaseClsEnum.MANUFACTURED.V)) {
    _trigger = watch(
      compRef,
      () => {
        if (compRef.value && _trigger && compRef.value.setFormCallback) {
          compRef.value?.setFormCallback(form)
          _trigger()
        }
      },
      { immediate: true }
    )
  }
}

FORM.HOOK.beforeValidateCU = () => {
  isFold.value = false
}

FORM.HOOK.beforeSubmit = () => {
  if (!compRef.value?.validate() && !form.boolUsed) return false
}

FORM.HOOK.afterSubmit = () => {
  emit('success')
}

// 表单提交数据清理
cu.submitFormFormat = async (form) => {
  form.attachmentIds = form.attachments ? form.attachments.map((v) => v.id) : undefined
  form.auxMaterialIds = form.isAllMaterial ? [0] : form.auxMaterialIds
  form.otherMaterialIds = form.isAllOtherMaterial ? [0] : form.otherMaterialIds
  if (form.useRequisitions) {
    form.applyPurchaseIds = form.actualRequisitionIds
  }
  if (!isManuf.value) {
    form.list = await numFmtByBasicClass(form.list, { toSmallest: true, toNum: true })
  } else {
    form.projectIds = [form.projectId]
    form.list = []
    if (compRef?.value) {
      const _list = compRef?.value.fetchResList()
      _list.forEach((v) => {
        // v.mergeIds.forEach((id) => {
        const { curPurchaseQuantity: quantity, basicClass } = form.manufListObj[v.rowKey]
        form.list.push({
          artifactEnclosureId: v.id,
          quantity,
          basicClass,
          pricingMethod: v.pricingMethod,
          unitPrice: v.unitPrice,
          amount: v.amount,
          destination: v.destination
        })
        // })
      })
    }
  }
  return form
}

async function submit() {
  try {
    await cu.submit()
  } catch (error) {
    console.log('采购合同提交', error)
  }
}
</script>

<style lang="scss" scoped>
::-webkit-scrollbar {
  /*滚动条整体样式*/
  width: 4px; /*高宽分别对应横竖滚动条的尺寸*/
  height: 4px;
}

.main-content {
  ::v-deep(.input-underline input) {
    text-align: left;
  }
}

.form-content {
  display: flex;
  flex-direction: row;
  justify-content: flex-start;
  align-items: flex-start;
}
.form-left {
  width: 455px;
  flex: none;
  height: 100%;
  overflow: auto;
  padding-right: 20px;
  box-sizing: border-box;
  transition: all 0.3s;
}
.vertical-dashed-divider {
  display: block;
  margin: 0 16px 0 1px;
  transition: all 0.3s;
}

.form-right {
  width: 100%;
  height: 100%;
  overflow-y: auto;
  overflow-x: hidden;
  position: relative;

  .hamburger-container {
    position: absolute;
    padding: 0;
    left: 0px;
    top: 12px;
    cursor: pointer;
    transform: scale(1.2);
    opacity: 0.6;
  }

  .el-table {
    ::v-deep(td > .cell) {
      min-height: 30px;
      line-height: 30px;
    }
  }

  .right-head {
    height: 45px;
    padding-left: 32px;
    margin-top: 10px;
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
    .opt-content {
      flex: none;
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
</style>
