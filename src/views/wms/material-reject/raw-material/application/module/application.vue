<template>
  <common-drawer
    ref="drawerRef"
    v-model="dialogVisible"
    :content-loading="detailLoading"
    :before-close="handleClose"
    :title="drawerTitle"
    show-close
    size="100%"
    custom-class="raw-mat-reject-application-form"
  >
    <template #titleAfter>
      <title-after-info :order="order" :detail="detail" />
    </template>
    <template #titleRight>
      <el-badge :value="curAddRecordNumber" :hidden="curAddRecordNumber < 1">
        <common-button size="mini" type="primary" @click="previewVisible = true">本次退货预览</common-button>
      </el-badge>
      <purchase-detail-button v-permission="permission.purchaseOrderDetail" :purchase-id="order.id" size="mini" />
    </template>
    <template #content>
      <common-table
        :data="detail.list"
        :max-height="maxHeight"
        show-summary
        :summary-method="getSummaries"
        :expand-row-keys="expandRowKeys"
        row-key="id"
      >
        <el-expand-table-column :data="detail.list" v-model:expand-row-keys="expandRowKeys" row-key="id">
          <template #default="{ row }">
            <div v-if="isNotBlank(row.allRejectList)" class="flex-rcc mtb-20">
              <reject-info-table
                :stripe="false"
                :material="row"
                :basic-class="row.basicClass"
                :list="row.allRejectList"
                operable
                @del="handleRejectDel"
              />
            </div>
          </template>
        </el-expand-table-column>
        <!-- 基础信息 -->
        <material-base-info-columns :basic-class="detail.basicClass" show-reject-status />
        <!-- 次要信息 -->
        <material-secondary-info-columns :basic-class="detail.basicClass" />
        <!-- 单位及其数量 -->
        <material-unit-quantity-columns :basic-class="detail.basicClass" />
        <!-- 价格信息 -->
        <template v-if="showAmount">
          <amount-info-columns v-if="!boolPartyA" />
        </template>
        <warehouse-info-columns show-project />
        <el-table-column label="操作" width="100" align="center" fixed="right">
          <template #default="{ row }">
            <common-button
              v-if="row.rejectStatus !== materialRejectStatusEnum.ALL.V"
              type="warning"
              size="mini"
              @click="openMatchListDlg(row)"
            >
              退货
            </common-button>
          </template>
        </el-table-column>
      </common-table>
    </template>
  </common-drawer>
  <reject-match-info
    v-model:visible="rejectMatchVisible"
    :material="currentRowMaterial"
    :reject-info="currentRowRejectInfo"
    @change="handRejectChange(currentRowMaterial)"
  />
  <preview
    v-if="detail.id"
    v-model:visible="previewVisible"
    :reject-info="rejectInfo"
    :basic-class="detail.basicClass"
    :inbound-id="detail.id"
    @success="handleSubmitSuccess"
  />
</template>

<script setup>
import { getInboundDetail } from '@/api/wms/material-reject/raw-material/application'
import { computed, inject, ref, defineProps, defineEmits, watch } from 'vue'
import { materialRejectStatusEnum, measureTypeEnum, orderSupplyTypeEnum } from '@enum-ms/wms'
import { reviewStatusEnum } from '@/utils/enum/modules/common'
import { materialStatusEnum } from '@/views/wms/material-reject/enum'
import { tableSummary } from '@/utils/el-extra'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { deepClone, isNotBlank, toFixed } from '@/utils/data-type'
import { obj2arr } from '@/utils/convert/type'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@/composables/use-visible'
import useWmsConfig from '@/composables/store/use-wms-config'
import elExpandTableColumn from '@comp-common/el-expand-table-column.vue'
import materialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import materialUnitQuantityColumns from '@/components-system/wms/table-columns/material-unit-quantity-columns/index.vue'
import materialSecondaryInfoColumns from '@/components-system/wms/table-columns/material-secondary-info-columns/index.vue'
import amountInfoColumns from '@/components-system/wms/table-columns/amount-info-columns/index.vue'
import warehouseInfoColumns from '@/components-system/wms/table-columns/warehouse-info-columns/index.vue'
import titleAfterInfo from '@/views/wms/material-reject/raw-material/components/title-after-info.vue'
import purchaseDetailButton from '@/components-system/wms/purchase-detail-button/index.vue'
import RejectInfoTable from '@/views/wms/material-reject/raw-material/components/reject-info-table.vue'
import RejectMatchInfo from '@/views/wms/material-reject/raw-material/components/reject-match-info.vue'
import Preview from './preview.vue'

const emit = defineEmits(['success', 'update:visible'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  inboundId: {
    type: Number
  }
})

const permission = inject('permission')
const drawerRef = ref()
// 展开行
const expandRowKeys = ref([])
// 详情加载
const detailLoading = ref(false)
// 详情
const detail = ref({})
// 退货匹配显示
const rejectMatchVisible = ref(false)
// 退货列表预览
const previewVisible = ref(false)
// 当前物料行
const currentRowMaterial = ref()
// 退货物料汇总信息
const rejectInfo = ref({})
// 当前行物料 退货信息
const currentRowRejectInfo = ref()
// 本次退货总记录数
const curAddRecordNumber = ref(0)

const { visible: dialogVisible, handleClose } = useVisible({ emit, props, field: 'visible' })

// 表格高度处理
const { maxHeight } = useMaxHeight(
  {
    mainBox: '.raw-mat-reject-application-form',
    extraBox: ['.el-drawer__header'],
    wrapperBox: ['.el-drawer__body'],
    clientHRepMainH: true,
    minHeight: 300
  },
  dialogVisible
)

// 退货配置
const { rejectCfg } = useWmsConfig()

// 物料金额显示
const materialAmountDisplayWay = computed(() => {
  return rejectCfg.value ? rejectCfg.value.materialAmountDisplayWay : {}
})
// 显示金额
const showAmount = computed(() => !!materialAmountDisplayWay.value.application)
// 采购订单信息
const order = computed(() => detail.value.purchaseOrder || {})
// 是否甲供订单
const boolPartyA = computed(() => order.value.supplyType === orderSupplyTypeEnum.PARTY_A.V)
// 标题
const drawerTitle = computed(() =>
  detailLoading.value ? `入库单` : `入库单：${detail.value.serialNumber}（ ${order.value.supplier ? order.value.supplier.name : ''} ）`
)

watch(dialogVisible, (visible) => {
  // 每次打开刷新
  if (visible) {
    init()
    fetchInboundDetail(props.inboundId)
  }
})

// 显示匹配列表
function openMatchListDlg(row) {
  rejectMatchVisible.value = true
  currentRowMaterial.value = row
  currentRowRejectInfo.value = rejectInfo.value[row.id].KV
}

// 初始化
function init() {
  detailLoading.value = false
  detail.value = {}
  rejectInfo.value = {}
  currentRowRejectInfo.value = undefined
  currentRowMaterial.value = undefined
  expandRowKeys.value = []
}

// 加载入库详情
async function fetchInboundDetail(id) {
  if (!id) return
  try {
    detailLoading.value = true
    const detailInfo = await getInboundDetail(id)
    detail.value = await detailFormat(detailInfo)
    // 有退库数据展开处理
    detail.value.list.forEach((row) => {
      rejectInfo.value[row.id] = { KV: {}, material: row }
      // 总数
      row.totalNumber = row.outboundUnitType === measureTypeEnum.MEASURE.V ? row.quantity : row.mete
      // 退货总数
      row.rejectNumber = 0
      // 待退货总数
      row.rejectPendingNumber = 0
      if (Array.isArray(row.rejectList)) {
        row.rejectList.forEach((rr) => {
          const rejectMaterialInfo = rr.material
          let number = row.outboundUnitType === measureTypeEnum.MEASURE.V ? rejectMaterialInfo.quantity : rejectMaterialInfo.mete
          number = +toFixed(number, row.outboundUnitPrecision)
          // 待审核状态
          if (rr.reviewStatus === reviewStatusEnum.UNREVIEWED.V) {
            row.rejectPendingNumber += number
          }
          // 已退货状态
          if (rr.reviewStatus === reviewStatusEnum.PASS.V) {
            row.rejectNumber += number
          }
        })
      }
      row.rejectNumber = +toFixed(row.rejectNumber, row.outboundUnitPrecision)
      row.rejectPendingNumber = +toFixed(row.rejectPendingNumber, row.outboundUnitPrecision)
      // 当前最大可退货数量
      row.rejectMaxNumber = +toFixed(row.totalNumber - row.rejectNumber - row.rejectPendingNumber, row.outboundUnitPrecision)
    })
  } catch (error) {
    console.error('退库-获取入库详情', error)
  } finally {
    detailLoading.value = false
  }
}

// 详情格式转换
async function detailFormat(detail) {
  await setSpecInfoToList(detail.list)
  await numFmtByBasicClass(detail.list)
  // 退货信息转换
  const rejectList = []
  detail.list.forEach((row) => {
    if (Array.isArray(row.rejectList)) {
      row.rejectList.forEach((rr) => {
        rejectList.push(rr.material)
      })
      row.allRejectList = [...row.rejectList]
    } else {
      row.rejectList = []
    }
  })
  await setSpecInfoToList(rejectList)
  await numFmtByBasicClass(rejectList)
  return detail
}

// 发生退库变化
function handRejectChange(row) {
  const index = expandRowKeys.value.findIndex((key) => key === row.id)
  if (index === -1) {
    expandRowKeys.value.push(row.id)
  }
  // 当前退货列表
  const curRejectList = obj2arr(rejectInfo.value[row.id].KV)
  // 历史退货+当前退货列表
  row.allRejectList = row.rejectList.concat(
    curRejectList.map((rj) => {
      const _material = deepClone(rj)
      // 存在计量单位，设置计量量
      if (row.measureUnit) {
        _material.quantity =
          row.outboundUnitType === measureTypeEnum.MEASURE.V
            ? rj.rejectNumber
            : toFixed(rj.rejectNumber * row.accountingUnitNet, row.measurePrecision)
      }
      // 设置核算量
      _material.mete =
        row.outboundUnitType === measureTypeEnum.ACCOUNTING.V
          ? rj.rejectNumber
          : toFixed(rj.rejectNumber * row.unitNet, row.accountingPrecision)

      return { reviewStatus: materialStatusEnum.UNSUBMITTED.V, material: _material }
    })
  )
}

// 处理退货删除
function handleRejectDel(material, rejectRow) {
  const rj = rejectInfo.value[material.id].KV
  delete rj[rejectRow.material.id]
}

// 提交成功
function handleSubmitSuccess() {
  handleClose()
  emit('success')
}

// 合计
function getSummaries(param) {
  return tableSummary(param, {
    props: ['quantity', 'mete', 'amount', 'amountExcludingVAT', 'inputVAT'],
    toThousandFields: ['amount', 'amountExcludingVAT', 'inputVAT']
  })
}
</script>
