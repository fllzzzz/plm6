<template>
  <common-drawer
    ref="drawerRef"
    :visible="dialogVisible"
    :contentLoading="crud.editDetailLoading"
    :before-close="crud.cancelCU"
    :title="crud.status.title"
    :show-close="true"
    size="100%"
    :close-on-click-modal="false"
    custom-class="purchase-order-raw-mat-form"
  >
    <template #titleRight>
      <common-button :loading="crud.status.cu === CRUD.STATUS.PROCESSING" size="mini" type="primary" @click="crud.submitCU">
        提 交
      </common-button>
      <store-operation v-if="crud.status.add > CRUD.STATUS.NORMAL" type="crud" />
    </template>
    <template #content>
      <div class="main-content">
        <el-form ref="formRef" :model="form" :rules="rules" size="small" label-position="right" label-width="110px">
          <div class="form-content" :style="heightStyle">
            <div class="form-left">
              <div class="order-details">
                <el-form-item label="是否绑定申购" prop="useRequisitions" required>
                  <common-radio v-model="form.useRequisitions" :disabled="Boolean(form.boolUsed)" :options="whetherEnum.ENUM" type="enum" />
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
                    :options="materialPurchaseClsEnum.ENUM"
                    type="enum"
                  />
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
                    mode="contain"
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
                <el-form-item class="el-form-item-8" label="合同量" prop="mete">
                  <div class="input-underline flex-rss child-mr-10">
                    <common-input-number
                      v-model="form.mete"
                      placeholder="请填写合同量"
                      autocomplete="off"
                      :max="999999999999"
                      :step="1"
                      :controls="false"
                      style="width: 100%"
                    />
                    <unit-select
                      v-model="form.meteUnit"
                      size="small"
                      :disabled="Boolean(form.boolUsed)"
                      clearable
                      filterable
                      style="width: 80px; flex: none"
                    />
                  </div>
                </el-form-item>
                <el-form-item class="el-form-item-9" label="合同额" prop="amount">
                  <div class="input-underline flex-rss child-mr-10">
                    <common-input-number
                      v-model="form.amount"
                      :max="999999999999"
                      :precision="2"
                      :step="1"
                      :controls="false"
                      placeholder="请填写合同额"
                      autocomplete="off"
                      class="input-underline"
                      style="width: 100%"
                    />
                    <span>元</span>
                  </div>
                </el-form-item>
                <el-form-item class="el-form-item-10" label="发票及税率" prop="invoiceType">
                  <invoice-type-select
                    class="input-underline"
                    v-model:invoiceType="form.invoiceType"
                    v-model:taxRate="form.taxRate"
                    :disabled="Boolean(form.boolUsed)"
                    :classification="form.basicClass"
                  />
                </el-form-item>
                <el-form-item class="el-form-item-11" prop="weightMeasurementMode" label="计量方式">
                  <common-radio
                    v-model="form.weightMeasurementMode"
                    :options="weightMeasurementModeEnum.ENUM"
                    type="enum"
                    :props="{ key: 'K', label: 'SL', value: 'V' }"
                    :disabled="Boolean(form.boolUsed)"
                    :size="'small'"
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
            <div class="vertical-dashed-divider" />
            <div class="form-right">
              <div class="right-head flex-rbs" v-if="form.useRequisitions">
                <!-- 关联申购单-->
                <span class="right-head-content">
                  <span class="label">关联申购单号</span>
                  <common-radio-button
                    v-if="form.materialType & materialPurchaseClsEnum.STEEL.V"
                    type="enum"
                    v-model="form.currentBasicClass"
                    :options="steelClsEnum.ENUM"
                    :disabledVal="[]"
                    clearable
                    style="vertical-align: middle; margin-right: 6px"
                  >
                    <template #suffix="{ item }">
                      <span v-if="form[compListVK[item.V]]?.length">({{ form[compListVK[item.V]]?.length }})</span>
                    </template>
                  </common-radio-button>
                  <el-tag v-for="item in form.requisitions" :key="item.id" effect="plain" class="preparation-sn-tag">
                    {{ item.serialNumber }}
                  </el-tag>
                </span>
                <span class="opt-content">
                  <common-button type="success" size="mini" @click="addRequisition"> 添加物料 </common-button>
                </span>
              </div>
              <div class="right-head flex-rbs" v-if="!form.useRequisitions">
                <!-- 自建采购清单 -->
                <span class="right-head-content">
                  <span class="label">采购清单</span>
                  <common-radio-button
                    v-if="form.materialType & materialPurchaseClsEnum.STEEL.V"
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
                <span class="opt-content">
                  <common-button v-if="!isManuf" type="success" size="mini" @click="materialSelectVisible = true"> 添加物料 </common-button>
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
              <component ref="compRef" :is="currentView" />
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
        append-to-body
        v-model="purchaseManufVisible"
        custom-class="manufactured-select-drawer"
      >
        <template #content>
          <manuf-list :project-id="form.projectId" :maxHeight="manufSelectMaxHeight" @add="handleAddManuf" />
        </template>
      </common-drawer>
      <!-- <submit-preview v-model:visible="previewVisible" :loading="crud.status.cu === CRUD.STATUS.PROCESSING" @submit="crud.submitCU" /> -->
    </template>
  </common-drawer>
</template>

<script setup>
import { ref, computed, provide, nextTick, watchEffect, watch } from 'vue'
import { matClsEnum, steelClsEnum, materialPurchaseClsEnum } from '@enum-ms/classification'
import { orderSupplyTypeEnum, baseMaterialTypeEnum, purchaseOrderPaymentModeEnum } from '@enum-ms/wms'
import { logisticsPayerEnum, logisticsTransportTypeEnum } from '@/utils/enum/modules/logistics'
import { weightMeasurementModeEnum, invoiceTypeEnum } from '@enum-ms/finance'
import { fileClassifyEnum } from '@enum-ms/file'
import { whetherEnum } from '@enum-ms/common'
import { steelInboundFormFormat } from '@/utils/wms/measurement-calc'
import { isNotBlank, isBlank, toPrecision, deepClone } from '@/utils/data-type'
import { clearObject } from '@/utils/data-type/object'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { arr2obj, obj2arr } from '@/utils/convert/type'

import { regForm } from '@compos/use-crud'
import UnitSelect from '@comp-common/unit-select/index.vue'
import ProjectCascader from '@comp-base/project-cascader.vue'
import SupplierSelect from '@comp-base/supplier-select/index.vue'
import BranchCompanySelect from '@comp-base/branch-company-select.vue'
import InvoiceTypeSelect from '@/components-system/base/invoice-type-select.vue'
import UploadList from '@comp/file-upload/UploadList.vue'
import StoreOperation from '@crud/STORE.operation.vue'
import useMaxHeight from '@/composables/use-max-height'

import RequisitionListApplication from '../components/requisition-list/index.vue'
import ManufList from '../components/manuf-list.vue'
// import SubmitPreview from '../components/submit-preview.vue'
import SteelApplication from '../components/application/steel/index'
import AuxMatApplication from '../components/application/auxiliary-material/index'
import ManufApplication from '../components/application/manufactured/index'
import materialTableSpecSelect from '@/components-system/classification/material-table-spec-select.vue'

const defaultForm = {
  useRequisitions: false, // 是否绑定申购单
  serialNumber: undefined, // 采购合同编号编号
  supplyType: orderSupplyTypeEnum.SELF.V, // 供货类型
  materialType: materialPurchaseClsEnum.STEEL.V, // 材料类型
  purchaseType: baseMaterialTypeEnum.RAW_MATERIAL.V, // 物料种类
  currentBasicClass: matClsEnum.STEEL_PLATE.V, // 物料类型
  isAllMaterial: false, // 是否选择全部辅材
  auxMaterialIds: undefined, // 辅材明细ids
  projectIds: undefined, // 项目ids
  projectId: undefined,
  requisitionsSN: undefined, // 申购单编号
  supplierId: undefined, // 供应商id
  branchCompanyId: undefined, // 公司签订主体
  mete: undefined, // 合同量
  amount: undefined, // 合同金额
  invoiceType: invoiceTypeEnum.SPECIAL.V, // 发票类型
  taxRate: undefined, // 税率
  weightMeasurementMode: weightMeasurementModeEnum.THEORY.V, // 重量计量方式
  logisticsTransportType: logisticsTransportTypeEnum.FREIGHT.V, // 物流运输方式
  logisticsPayerType: logisticsPayerEnum.SUPPLIER.V, // 物流运输方式 90%由供方承担运费
  purchaseOrderPaymentMode: purchaseOrderPaymentModeEnum.ARRIVAL.V, // 订单类型
  remark: undefined, // 备注
  attachments: undefined, // 附件
  attachmentIds: undefined, // 附件ids
  list: [],
  sectionSteelList: [],
  steelPlateList: [],
  steelCoilList: [],
  requisitions: [],
  requisitionsKV: {},
  manufListObj: {},
  manufMergeObj: {}
}

const formRef = ref() // 表单

const { CRUD, crud, form } = regForm(defaultForm, formRef)
const dialogVisible = computed(() => crud.status.cu > CRUD.STATUS.NORMAL)
// const previewVisible = ref(false)

// 表格高度处理
const { maxHeight, heightStyle } = useMaxHeight(
  {
    mainBox: '.purchase-order-raw-mat-form',
    extraBox: ['.el-drawer__header'],
    wrapperBox: ['.el-drawer__body'],
    clientHRepMainH: true
  },
  dialogVisible
)

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
  weightMeasurementMode: [{ required: true, message: '请选择计量方式', trigger: 'change' }],
  logisticsTransportType: [{ required: true, message: '请选择物流运输方式', trigger: 'change' }],
  logisticsPayerType: [{ required: true, message: '请选择物流费用承担方', trigger: 'change' }],
  purchaseOrderPaymentMode: [{ required: true, message: '请选择订单类型', trigger: 'change' }],
  invoiceType: [{ required: true, validator: validateInvoiceType, trigger: 'change' }],
  taxRate: [{ max: 2, message: '请输入税率', trigger: 'blur' }],
  mete: [{ required: true, message: '请填写合同量', trigger: 'blur' }],
  amount: [{ required: true, message: '请填写合同额', trigger: 'blur' }]
}

// rules变更
watchEffect(() => {
  if (dialogVisible.value) {
    const r = rules.value
    clearObject(r)
    Object.assign(r, baseRules)
    Object.assign(r, selfRules)
    nextTick(() => {
      formRef.value && formRef.value.clearValidate()
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
      form.currentBasicClass = matClsEnum.MATERIAL.V
    }
    if (val & materialPurchaseClsEnum.STEEL.V) {
      form.currentBasicClass = matClsEnum.STEEL_PLATE.V
    }
    if (val & materialPurchaseClsEnum.MANUFACTURED.V) {
      form.currentBasicClass = matClsEnum.STRUC_MANUFACTURED.V
      form.purchaseType = baseMaterialTypeEnum.MANUFACTURED.V
    } else {
      form.purchaseType = baseMaterialTypeEnum.RAW_MATERIAL.V
    }
    form.list = []
    form.sectionSteelList = []
    form.steelPlateList = []
    form.steelCoilList = []
    form.requisitionsKV = {}
    form.manufListObj = {}
    // form.manufMergeObj = {}
    if (matSpecRef.value) {
      matSpecRef.value.clear()
    }
  }
)

const currentView = computed(() => {
  switch (form.materialType) {
    case materialPurchaseClsEnum.STEEL.V:
      return SteelApplication
    case materialPurchaseClsEnum.MATERIAL.V:
      return AuxMatApplication
    case materialPurchaseClsEnum.MANUFACTURED.V:
      return ManufApplication
    default:
      return SteelApplication
  }
})

// --------------------------- 自选采购 start ------------------------------

const compListVK = {
  [matClsEnum.STEEL_PLATE.V]: 'steelPlateList',
  [matClsEnum.SECTION_STEEL.V]: 'sectionSteelList',
  [matClsEnum.STEEL_COIL.V]: 'steelCoilList',
  [matClsEnum.MATERIAL.V]: 'list',
  [matClsEnum.STRUC_MANUFACTURED.V]: 'list',
  [matClsEnum.ENCL_MANUFACTURED.V]: 'list'
}

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
// 添加制成品
function handleAddManuf(list) {
  for (let i = 0; i < list.length; i++) {
    const v = deepClone(list[i])
    v.measureUnit = '件' // 计量单位
    v.accountingUnit = '千克' // 核算单位
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
}

// --------------------------- 申购 start ------------------------------

const requisitionListRef = ref()
const requisitionVisible = ref(false)

// const requisitionBadge = computed(() => (form.requisitionListKV ? Object.keys(form.requisitionListKV)?.length : 0))

watchEffect(() => {
  form.requisitions = obj2arr(form.requisitionsKV)
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

// 初始化表单
CRUD.HOOK.afterToAdd = () => {}

CRUD.HOOK.beforeToCU = () => {}

// 加载处理
CRUD.HOOK.beforeEditDetailLoaded = async (crud, form) => {
  if (isNotBlank(form.projects)) {
    form.projectIds = form.projects.map((v) => v.id)
  }
  if (isBlank(form.attachments)) {
    form.attachments = []
  }
  // 是否绑定申购
  form.useRequisitions = isNotBlank(form.applyPurchase)
  if (form.useRequisitions) {
    form.requisitionsKV = arr2obj(form.applyPurchase)
  }
  // 是否选中所有辅材，0表示所有
  form.isAllMaterial = form.auxMaterialIds?.includes(0)
  // 签订主体id
  form.branchCompanyId = form.branchCompany ? form.branchCompany.id : undefined
  // 供应商id
  form.supplierId = form.supplier ? form.supplier.id : undefined
  if (form.materialType & materialPurchaseClsEnum.MANUFACTURED.V) {
    form.manufListObj = {}
    // form.manufMergeObj = {}
    const _list = form.details.map((v) => {
      v.curPurchaseQuantity = v.quantity
      v.detailId = v.id
      v.id = v.artifactEnclosureId
      return v
    })
    handleAddManuf(_list)
  } else {
    await setSpecInfoToList(form.details)
    form.list = await numFmtByBasicClass(form.details, {
      toNum: true
    })
    if (form.materialType & materialPurchaseClsEnum.STEEL.V) {
      // 修改的情况下，数据预处理
      await steelInboundFormFormat(form)
    }
    compRef.value?.setFormCallback(form)
  }
  // form.currentBasicClass设置初始值
  for (const item in matClsEnum.ENUM) {
    if (matClsEnum[item].V & form.basicClass) {
      form.currentBasicClass = matClsEnum[item].V
      break
    }
  }
}

CRUD.HOOK.beforeSubmit = (crud, form) => {
  if (!compRef.value?.validate()) return false
  // if (!previewVisible.value) {
  //   previewVisible.value = true
  //   return false
  // }
}

// CRUD.HOOK.afterSubmit = (crud, form) => {
//   previewVisible.value = false
// }

// 表单提交数据清理
crud.submitFormFormat = async (form) => {
  form.attachmentIds = form.attachments ? form.attachments.map((v) => v.id) : undefined
  form.auxMaterialIds = form.isAllMaterial ? [0] : form.auxMaterialIds
  if (form.useRequisitions) {
    form.applyPurchaseIds = form.requisitions.map((v) => v.id)
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
}
.vertical-dashed-divider {
  margin: 0 16px 0 1px;
}

.form-right {
  width: 100%;
  height: 100%;
  overflow-y: auto;
  overflow-x: hidden;

  .el-table {
    ::v-deep(td > .cell) {
      min-height: 30px;
      line-height: 30px;
    }
  }

  .right-head {
    height: 45px;
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
}
</style>
