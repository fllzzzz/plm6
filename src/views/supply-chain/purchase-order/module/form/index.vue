<template>
  <common-drawer
    ref="drawerRef"
    :visible="dialogVisible"
    :contentLoading="crud.editDetailLoading"
    :before-close="crud.cancelCU"
    :title="crud.status.title"
    destroy-on-close
    :show-close="true"
    :size="formSize"
    :close-on-click-modal="false"
    custom-class="purchase-order-raw-mat-form"
  >
    <template #titleAfter>
    </template>
    <template #content>
      <div>
         <component v-if="crud.status.cu > CRUD.STATUS.NORMAL" :is="comp" :detail="form" :edit="isEdit" @success="handleSuccess" :dialogVisible="dialogVisible" :maxHeight="maxHeight-50">
          <template #chose>
            <div class="el-form-item el-form-item--small" v-if="!isEdit">
              <div class="el-form-item__label" style="font-weight:700;font-size:14px;width:110px;"><span style="color:#f56c6c;font-weight:normal;margin-right:4px;">*</span>是否有明细</div><common-radio v-model="type" :options="whetherEnum.ENUM" type="enum" @change="typeChange"/>
            </div>
          </template>
        </component>
      </div>
    </template>
  </common-drawer>
</template>

<script setup>
import { ref, computed, watch } from 'vue'
import { regForm } from '@compos/use-crud'

import { orderSupplyTypeEnum, baseMaterialTypeEnum, purchaseOrderPaymentModeEnum } from '@enum-ms/wms'
import { logisticsPayerEnum, logisticsTransportTypeEnum } from '@/utils/enum/modules/logistics'
import { weightMeasurementModeEnum, invoiceTypeEnum } from '@enum-ms/finance'
import { whetherEnum } from '@enum-ms/common'
import { matClsEnum, materialPurchaseClsEnum } from '@enum-ms/classification'
import { steelInboundFormFormat } from '@/utils/wms/measurement-calc'
import { isNotBlank, isBlank, deepClone, toPrecision } from '@/utils/data-type'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { arr2obj } from '@/utils/convert/type'
import useWmsConfig from '@/composables/store/use-wms-config'
import useMaxHeight from '@/composables/use-max-height'

import hasDetailForm from './has-detail-form'
import rawMaterial from './raw-material'

const defaultForm = {
  useRequisitions: true, // 是否绑定申购单
  serialNumber: undefined, // 采购合同编号编号
  supplyType: orderSupplyTypeEnum.SELF.V, // 供货类型
  // materialType: materialPurchaseClsEnum.STEEL.V, // 材料类型
  purchaseType: baseMaterialTypeEnum.RAW_MATERIAL.V, // 物料种类
  currentBasicClass: matClsEnum.STEEL_PLATE.V, // 物料类型
  isAllMaterial: false, // 是否选择全部辅材
  auxMaterialIds: undefined, // 辅材明细ids
  isAllOtherMaterial: false, // 是否选择全部其他材料
  otherMaterialIds: undefined, // 其他材料明细ids
  projectIds: undefined, // 项目ids
  projectId: undefined,
  supplierId: undefined, // 供应商id
  branchCompanyId: undefined, // 公司签订主体
  mete: undefined, // 合同量
  meteUnit: 'kg', // 合同量单位
  amount: undefined, // 合同金额
  invoiceType: invoiceTypeEnum.SPECIAL.V, // 发票类型
  taxRate: undefined, // 税率
  // weightMeasurementMode: weightMeasurementModeEnum.THEORY.V, // 重量计量方式
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
  actualRequisitionIds: [],
  manufListObj: {},
  manufMergeObj: {}
}

const { purchaseCfg: currentCfg } = useWmsConfig()
const formSize = ref('620px')

const formRef = ref() // 表单
const type = ref()

const { CRUD, crud, form } = regForm(defaultForm, formRef)
const dialogVisible = computed(() => crud.status.cu > CRUD.STATUS.NORMAL)
// 是否是编辑状态
const isEdit = computed(() => {
  return crud.status.edit > 0
})

watch(
  () => dialogVisible.value,
  (val) => {
    if (val) {
      formSize.value = '620px'
      if (!form.id) {
        type.value = isNotBlank(currentCfg.value.boolHaveDetail) ? currentCfg.value.boolHaveDetail : true
        if (type.value === whetherEnum.TRUE.V) {
          formSize.value = '100%'
        }
      }
    }
  },
  { immediate: true, deep: true }
)

function typeChange(val) {
  formSize.value = val ? '100%' : '620px'
}

const comp = computed(() => {
  switch (type.value) {
    case whetherEnum.TRUE.V:
      return hasDetailForm
    default:
      return rawMaterial
  }
})

// // 是否有关联备料单
// const hasAssocPreparation = computed(() => {
//   return form.preparationSNList && form.preparationSNList.length > 0
// })

// 表格高度处理
const { maxHeight } = useMaxHeight(
  {
    mainBox: '.purchase-order-raw-mat-form',
    extraBox: ['.el-drawer__header'],
    wrapperBox: ['.el-drawer__body'],
    clientHRepMainH: true
  },
  dialogVisible
)

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
}

// 加载处理
CRUD.HOOK.beforeEditDetailLoaded = async (crud) => {
  formSize.value = '620px'
  if (isNotBlank(form.projects)) {
    form.projectIds = form.projects.map((v) => v.id)
  }
  if (isBlank(form.attachments)) {
    form.attachments = []
  }
  // 是否绑定申购
  form.useRequisitions = isNotBlank(form.applyPurchase)
  let applyPurchaseObj = {}
  if (form.useRequisitions) {
    applyPurchaseObj = arr2obj(form.applyPurchase)
  }
  // 是否选中所有辅材，0表示所有
  form.isAllMaterial = form.auxMaterialIds?.includes(0)
  // 是否选中所有其它科目，0表示所有
  form.isAllOtherMaterial = form.otherMaterialIds?.includes(0)
  // 签订主体id
  form.branchCompanyId = form.branchCompany ? form.branchCompany.id : undefined
  // 供应商id
  form.supplierId = form.supplier ? form.supplier.id : undefined
  type.value = !!isNotBlank(form.details)
  if (isNotBlank(form.details)) {
    formSize.value = '100%'
    if (form.materialType & materialPurchaseClsEnum.MANUFACTURED.V) {
      form.projectId = form.projects?.length ? form.projects?.[0]?.id : undefined
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
      await numFmtByBasicClass(
        form.details,
        {
          toNum: true
        },
        {
          mete: ['mete', 'applyPurchaseMete']
        }
      )
      form.list = form.details.map((v) => {
        v.purchaseSN = applyPurchaseObj[v.applyPurchaseId]?.serialNumber
        v.purchaseTotalWeight = v.applyPurchaseMete
        return v
      })
      if (form.materialType & materialPurchaseClsEnum.STEEL.V) {
      // 修改的情况下，数据预处理
        await steelInboundFormFormat(form)
      }
    }

    Object.assign(form, { requisitionsKV: applyPurchaseObj })

    // form.currentBasicClass设置初始值
    for (const item in matClsEnum.ENUM) {
      if (matClsEnum[item].V & form.basicClass) {
        form.currentBasicClass = matClsEnum[item].V
        break
      }
    }
  }
  console.log(form)
}

function handleSuccess() {
  formSize.value = '620px'
  crud.cancelCU()
  crud.refresh()
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
