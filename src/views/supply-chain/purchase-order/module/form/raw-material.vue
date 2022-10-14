<template>
  <common-drawer
    ref="drawerRef"
    :visible="dialogVisible"
    :contentLoading="crud.editDetailLoading"
    :before-close="crud.cancelCU"
    :title="crud.status.title"
    :show-close="true"
    :size="hasAssocPreparation ? '100%' : '620px'"
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
                <el-form-item class="el-form-item-1" label="采购合同编号" prop="serialNumber">
                  <el-input
                    v-model.trim="form.serialNumber"
                    placeholder="可输入采购合同编号（不填写则自动生成）"
                    :disabled="form.boolUsed"
                    maxlength="30"
                    size="small"
                    class="input-underline"
                  />
                </el-form-item>
                <el-form-item class="el-form-item-3" label="物料种类" prop="basicClass">
                  <div class="flex-rss child-mr-10">
                    <basic-class-select
                      v-model="form.basicClass"
                      :type="form.purchaseType"
                      multiple
                      clearable
                      :disabled="form.boolUsed"
                      placeholder="请选择物料种类"
                      class="input-underline"
                      style="flex: auto"
                    />
                    <!-- 原材料才有是否甲供的概念 -->
                    <el-checkbox
                      v-model="form.boolPartyA"
                      :disabled="form.boolUsed"
                      label="甲供"
                      size="mini"
                      border
                      style="margin-top: 3px"
                      @change="boolPartyAChange"
                    />
                  </div>
                </el-form-item>
                <el-form-item v-if="form.basicClass & matClsEnum.MATERIAL.V" class="el-form-item-5" label="辅材明细" prop="auxMaterialIds">
                   <div class="flex-rss child-mr-10">
                    <!-- 是否选择所有辅材 -->
                    <el-checkbox
                      v-model="form.isAllMaterial"
                      :disabled="form.boolUsed"
                      label="所有辅材"
                      size="mini"
                      border
                      style="margin-top: 3px;margin-right:5px;"
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
                <el-form-item class="el-form-item-7" label="供应商" prop="supplierId">
                  <supplier-select
                    v-model="form.supplierId"
                    :basic-class="form.basicClass"
                    :type="form.purchaseType"
                    :disabled="form.boolUsed"
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
                    :disabled="form.boolUsed"
                    default
                    style="width: 100%"
                  />
                </el-form-item>
                <template v-if="!form.boolPartyA">
                  <el-form-item class="el-form-item-8" label="合同量" prop="mete" style="display: inline-flex; width: 320px">
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
                        :disabled="form.boolUsed"
                        clearable
                        filterable
                        style="width: 80px; flex: none"
                      />
                    </div>
                  </el-form-item>
                  <el-form-item
                    class="el-form-item-9"
                    label="合同额"
                    prop="amount"
                    style="display: inline-flex; width: 240px"
                    label-width="80px"
                  >
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
                      :disabled="form.boolUsed"
                      :classification="form.basicClass"
                    />
                  </el-form-item>
                  <el-form-item class="el-form-item-11" prop="weightMeasurementMode" label="计量方式">
                    <common-radio
                      v-model="form.weightMeasurementMode"
                      :options="weightMeasurementModeEnum.ENUM"
                      type="enum"
                      :props="{ key: 'K', label: 'SL', value: 'V' }"
                      :disabled="form.boolUsed"
                      :size="'small'"
                    />
                  </el-form-item>
                  <el-form-item class="el-form-item-13" label="订单类型" prop="purchaseOrderPaymentMode">
                    <common-radio
                      v-model="form.purchaseOrderPaymentMode"
                      :options="purchaseOrderPaymentModeEnum.ENUM"
                      type="enum"
                      :disabled="form.boolUsed"
                      :size="'small'"
                    />
                  </el-form-item>
                  <el-form-item class="el-form-item-16" label="物流运输方式" prop="logisticsTransportType">
                    <common-radio
                      v-model="form.logisticsTransportType"
                      :options="logisticsTransportTypeEnum.ENUM"
                      type="enum"
                      :disabled="form.boolUsed"
                      size="small"
                    />
                  </el-form-item>
                  <el-form-item class="el-form-item-17" label="物流费用承担" prop="logisticsPayerType">
                    <common-radio
                      v-model="form.logisticsPayerType"
                      :options="logisticsPayerEnum.ENUM"
                      type="enum"
                      :disabled="form.boolUsed"
                      size="small"
                    />
                  </el-form-item>
                </template>
                <el-form-item label="选择项目" class="el-form-item-4" prop="projectIds" label-width="80px">
                  <project-cascader v-model="form.projectIds" clearable :disabled="form.boolUsed" multiple class="input-underline" />
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
            <template v-if="hasAssocPreparation">
              <div class="vertical-dashed-divider" />
              <div class="form-right">
                <div class="right-head flex-rbs">
                  <!-- 关联备料单-->
                  <span class="assoc-preparation-list">
                    <span class="label">关联备料单</span>
                    <el-tag
                      v-for="serialNumber in form.preparationSNList"
                      :key="serialNumber"
                      :effect="currentPreparationSN === serialNumber ? 'dark' : 'plain'"
                      class="preparation-sn-tag"
                      @click="currentPreparationChange(serialNumber)"
                    >
                      {{ serialNumber }}
                    </el-tag>
                  </span>
                  <span v-if="currentPreparationSN" class="opt-content">
                    <common-button v-if="!sortingInfoMode" type="warning" size="mini" @click="toEditSorting"> 编辑分拣信息 </common-button>
                    <template v-else>
                      <common-button size="mini" @click.stop="cancelSortingInfo">取消编辑</common-button>
                      <common-button size="mini" type="success" @click.stop="saveSortingInfo">保存</common-button>
                    </template>
                  </span>
                </div>
                <!-- 清单列表 -->
                <common-table
                  ref="tableRef"
                  :data="sortingInfoMode ? currentSortingPreparationClone : preparationList"
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
                  <el-table-column v-if="sortingInfoMode" label="操作" width="80px" align="center" fixed="right">
                    <template #default="{ $index }">
                      <!-- 删除 -->
                      <common-button type="danger" icon="el-icon-delete" size="mini" @click.stop="removePreparationListRow($index)" />
                    </template>
                  </el-table-column>
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
import { ref, computed, nextTick, watchEffect } from 'vue'
import { matClsEnum } from '@enum-ms/classification'
import { orderSupplyTypeEnum, baseMaterialTypeEnum, purchaseOrderPaymentModeEnum } from '@enum-ms/wms'
import { logisticsPayerEnum, logisticsTransportTypeEnum } from '@/utils/enum/modules/logistics'
import { weightMeasurementModeEnum, invoiceTypeEnum } from '@enum-ms/finance'
import { fileClassifyEnum } from '@enum-ms/file'
import { isNotBlank, isBlank, deepClone } from '@/utils/data-type'
import { clearObject } from '@/utils/data-type/object'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'

import { regForm } from '@compos/use-crud'
import UnitSelect from '@comp-common/unit-select/index.vue'
import ProjectCascader from '@comp-base/project-cascader.vue'
import SupplierSelect from '@comp-base/supplier-select/index.vue'
import BranchCompanySelect from '@comp-base/branch-company-select.vue'
import MaterialCascader from '@comp-cls/material-cascader/index.vue'
import BasicClassSelect from '@/components-system/classification/basic-class-select.vue'
import InvoiceTypeSelect from '@/components-system/base/invoice-type-select.vue'
import UploadList from '@comp/file-upload/UploadList.vue'
import StoreOperation from '@crud/STORE.operation.vue'
import useMaxHeight from '@/composables/use-max-height'

import MaterialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import MaterialUnitQuantityColumns from '@/components-system/wms/table-columns/material-unit-quantity-columns/index.vue'
import MaterialSecondaryInfoColumns from '@/components-system/wms/table-columns/material-secondary-info-columns/index.vue'
import { ElMessage } from 'element-plus'

const defaultForm = {
  serialNumber: undefined, // 采购合同编号编号
  boolPartyA: false, // 是否甲供
  supplyType: orderSupplyTypeEnum.SELF.V, // 供货类型
  purchaseType: baseMaterialTypeEnum.RAW_MATERIAL.V, // 物料种类
  basicClass: null, // 物料类型
  isAllMaterial: false, // 是否选择全部辅材
  auxMaterialIds: undefined, // 辅材明细ids
  projectIds: undefined, // 项目ids
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
  attachmentIds: undefined // 附件ids
}

const formRef = ref() // 表单

const { CRUD, crud, form } = regForm(defaultForm, formRef)
const dialogVisible = computed(() => crud.status.cu > CRUD.STATUS.NORMAL)

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
  supplyType: [{ required: true, message: '请选择供货类型', trigger: 'change' }],
  purchaseType: [{ required: true, message: '请选择物料种类', trigger: 'change' }],
  basicClass: [{ required: true, message: '请选择物料类型', trigger: 'change' }],
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

// 甲供校验
const partyARules = {
  projectIds: [{ required: true, message: '请选择项目', trigger: 'change' }]
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

// rules变更
watchEffect(() => {
  if (dialogVisible.value) {
    const r = rules.value
    clearObject(r)
    Object.assign(r, baseRules)
    if (form.boolPartyA) {
      Object.assign(r, partyARules)
      // 甲供供应商选填
      delete r.supplierId
    } else {
      Object.assign(r, selfRules)
    }
    if (form.basicClass & matClsEnum.MATERIAL.V) {
      Object.assign(r, auxMatRules)
    }
    nextTick(() => {
      formRef.value && formRef.value.clearValidate()
    })
  }
})
// ------------------------- rules end -------------------------------------

// ------------------------- 备料/分拣 start -----------------------------------
// 是否有关联备料单
const hasAssocPreparation = computed(() => {
  return form.preparationSNList && form.preparationSNList.length > 0
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

// 分拣编辑模式
const sortingInfoMode = ref(false)
// 当前分拣的备料单：拷贝（避免影响源数据）
const currentSortingPreparationClone = ref([])
// ------------------------- 备料/分拣 end -----------------------------------

// 初始化表单
CRUD.HOOK.afterToAdd = () => {}

CRUD.HOOK.beforeToCU = () => {
  currentPreparationSN.value = undefined
  preparationListMap.value = new Map()
  mergePreparationList.value = []
}

// 加载处理
CRUD.HOOK.beforeEditDetailLoaded = async (crud, form) => {
  if (isNotBlank(form.projects)) {
    form.projectIds = form.projects.map((v) => v.id)
  }
  if (isBlank(form.attachments)) {
    form.attachments = []
  }
  // 是否选中所有辅材，0表示所有
  form.isAllMaterial = form.auxMaterialIds?.includes(0)
  // 是否甲供
  form.boolPartyA = form.supplyType === orderSupplyTypeEnum.PARTY_A.V
  // 签订主体id
  form.branchCompanyId = form.branchCompany ? form.branchCompany.id : undefined
  // 供应商id
  form.supplierId = form.supplier ? form.supplier.id : undefined
  if (Array.isArray(form.preparationList) && form.preparationList.length > 0) {
    // 获取规格信息以及单位转换
    await setSpecInfoToList(form.preparationList)
    await numFmtByBasicClass(form.preparationList, {
      toNum: true
    })
    // 遍历设置备料相关信息
    form.preparationList.forEach((row) => {
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

CRUD.HOOK.beforeSubmit = (crud, form) => {
  if (!checkHasSortingEdit()) return false
}

// 表单提交数据清理
crud.submitFormFormat = (form) => {
  form.attachmentIds = form.attachments ? form.attachments.map((v) => v.id) : undefined
  form.auxMaterialIds = form.isAllMaterial ? [0] : form.auxMaterialIds
  return form
}

// 当前选中的备料单号
function currentPreparationChange(preparationSN) {
  if (!checkHasSortingEdit()) return false
  if (preparationSN !== currentPreparationSN.value) {
    currentPreparationSN.value = preparationSN
  } else {
    currentPreparationSN.value = undefined
  }
}

// 校验分拣信息
function checkHasSortingEdit() {
  if (sortingInfoMode.value) {
    ElMessage.warning({ message: '请先“保存”或“取消编辑”分拣信息', duration: 4000 })
    return false
  }
  return true
}

// 编辑分拣信息
function toEditSorting() {
  sortingInfoMode.value = true
  currentSortingPreparationClone.value = deepClone(preparationList.value)
}

// 删除备料信息
function removePreparationListRow(data, index) {
  currentSortingPreparationClone.value.splice(index, 1)
}

// 取消编辑
function cancelSortingInfo() {
  sortingInfoMode.value = false
}

// 保存备料
function saveSortingInfo() {
  sortingInfoMode.value = false
  // 将修改后的分拣信息 替代 原来的分拣信息
  preparationListMap.value.set(currentPreparationSN.value, currentSortingPreparationClone.value)
}

// “是否甲供”状态变更
function boolPartyAChange(val) {
  form.supplyType = val ? orderSupplyTypeEnum.PARTY_A.V : orderSupplyTypeEnum.SELF.V
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
  width: 580px;
  flex: none;
  height: 100%;
  overflow: auto;
  // padding-right: 20px;
  box-sizing: border-box;
  .order-details {
    width: 560px;
  }
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
    .opt-content {
      flex: none;
    }
  }
}
</style>
