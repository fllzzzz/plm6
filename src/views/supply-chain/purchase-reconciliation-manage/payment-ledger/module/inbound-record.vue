<template>
  <common-drawer
    ref="dialogRef"
    title="入库记录"
    :close-on-click-modal="false"
    v-model="visible"
    direction="rtl"
    :before-close="handleClose"
    custom-class="collection-record"
    size="95%"
  >
    <template #titleAfter>
      <div>{{ detailInfo.serialNumber }}</div>
    </template>
    <template #titleRight>
      <div class="print-wrap">
        <!-- <print-table
          v-permission="props.permission?.print"
          api-key="projectCollectionDetail"
          :params="{ ...params }"
          size="mini"
          type="warning"
        /> -->
      </div>
    </template>
    <template #content>
      <common-table :data="list" :data-format="dataFormat" :max-height="maxHeight">
        <!-- 基础信息 -->
        <material-base-info-columns
          :columns="{}"
          spec-merge
        />
        <!-- 单位及其数量 -->
        <material-unit-quantity-columns :columns="{}" />
        <!-- 价格信息 -->
        <amount-info-columns :columns="{}" :show-tax-rate="true"/>
        <el-table-column prop="inputVat" label="税额" align="center" show-overflow-tooltip />
        <el-table-column prop="inboundTime" label="入库时间" align="center" show-overflow-tooltip />
        <el-table-column prop="inboundId" label="入库单号" align="center" show-overflow-tooltip />
        <el-table-column prop="invoiceAmount" label="入库人" align="center" show-overflow-tooltip />
        <el-table-column prop="invoiceUnit" label="审核人" align="center" show-overflow-tooltip />
      </common-table>
      <!--分页组件-->
      <el-pagination
        :total="total"
        :current-page="queryPage.pageNumber"
        :page-size="queryPage.pageSize"
        style="margin-top: 8px"
        layout="total, prev, pager, next, sizes"
        @size-change="handleSizeChange"
        @current-change="handleCurrentChange"
      />
    </template>
  </common-drawer>
</template>

<script setup>
import { inboundRecord } from '@/api/supply-chain/purchase-reconciliation-manage/payment-ledger'
import { ref, defineEmits, defineProps, watch, computed } from 'vue'
import MaterialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import MaterialUnitQuantityColumns from '@/components-system/wms/table-columns/material-unit-quantity-columns/index.vue'
import AmountInfoColumns from '@/components-system/wms/table-columns/amount-info-columns/index.vue'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import useVisible from '@/composables/use-visible'
import useMaxHeight from '@compos/use-max-height'
import usePagination from '@compos/use-pagination'

const emit = defineEmits(['update:modelValue'])

const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  },
  detailInfo: {
    type: Object,
    default: () => {}
  },
  permission: {
    type: Object,
    default: () => {}
  }
})

const { visible, handleClose } = useVisible({ emit, props })
const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetchList })

// 请求参数
const params = computed(() => {
  return {
    orderId: props.detailInfo.id
  }
})

watch(
  visible,
  (val) => {
    if (val) {
      fetchList()
    }
  }
)

const list = ref([])
const dialogRef = ref()
const tableLoading = ref(false)
const dataFormat = ref([
  ['inputVat', 'to-thousand'],
  ['inboundTime', ['parse-time', '{y}-{m}-{d}']]
])

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.inbound-record',
    extraBox: '.el-drawer__header',
    wrapperBox: '.el-drawer__body',
    extraHeight: '5vh',
    minHeight: 300,
    navbar: false,
    clientHRepMainH: true
  },
  dialogRef
)

// 获取入库记录
async function fetchList() {
  let _list = []
  tableLoading.value = true
  try {
    const { content = [], totalElements } = await inboundRecord({ ...params.value, ...queryPage })
    let hasContent = content
    if (hasContent.length > 0) {
      await setSpecInfoToList(hasContent)
      hasContent = await numFmtByBasicClass(hasContent)
    }
    _list = hasContent
    setTotalPage(totalElements)
  } catch (error) {
    console.log('获取入库记录失败', error)
  } finally {
    list.value = _list
    tableLoading.value = false
  }
}
</script>
