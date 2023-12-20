<template>
   <common-drawer
    ref="drawerRef"
    title="入库记录"
    :close-on-click-modal="false"
    v-model="visible"
    direction="rtl"
    :before-close="handleClose"
    custom-class="warehouse-record"
    size="90%"
  >
    <template #titleRight>
      <div class="print-wrap">
        <print-table
          v-permission="props.permission?.print"
          api-key="projectWarehouseRecord"
          :params="{ ...params }"
          size="mini"
          type="warning"
        />
      </div>
    </template>
    <template #content>
      <common-table :data="list" :data-format="dataFormat" :max-height="maxHeight">
        <el-table-column align="center" key="monomer.name" prop="monomer.name" label="单体" min-width="140" show-overflow-tooltip>
          <template #default="{ row }">
            <table-cell-tag :show="row.boolAllPartSendDirectly" name="檩条直发" color="#f56c6c" />
            <span v-empty-text>{{ row.monomer?.name }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="area.name" key="area.name" label="区域" min-width="140" show-overflow-tooltip />
        <el-table-column prop="name" key="name" label="名称" align="center" min-width="110" show-overflow-tooltip>
          <template #default="{row}">
            <table-cell-tag :show="row?.boolReturn" name="退量" color="#f56c6c"/>
            <span>{{ row.name }}</span>
        </template>
        </el-table-column>
        <el-table-column prop="serialNumber" key="serialNumber" label="编号" align="center" min-width="110" show-overflow-tooltip />
        <el-table-column prop="specification" key="specification" label="规格" align="center" min-width="120" show-overflow-tooltip />
        <el-table-column prop="material" key="material" label="材质" align="center" min-width="100" show-overflow-tooltip />
        <el-table-column prop="measureUnit" key="measure" label="计量单位" align="center" width="70" show-overflow-tooltip />
        <el-table-column prop="quantity" key="quantity" label="数量" align="center" min-width="60" show-overflow-tooltip />
        <el-table-column prop="accountingUnit" key="nuclear" label="核算单位" align="center" width="70" show-overflow-tooltip />
        <el-table-column prop="mete" key="mete" label="总量" align="center" min-width="100" show-overflow-tooltip />
        <el-table-column prop="unitPrice" key="unitPrice" label="单价" align="right" min-width="80" show-overflow-tooltip />
        <el-table-column prop="totalPrice" key="totalPrice" label="总价" align="right" min-width="100" show-overflow-tooltip />
        <el-table-column prop="createTime" key="createTime" label="入库时间"  align="center"  width="130" show-overflow-tooltip />
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
import { warehouseRecord } from '@/api/contract/sales-manage/order-tracking'
import { ref, defineEmits, defineProps, watch, computed } from 'vue'

import useVisible from '@/composables/use-visible'
import useMaxHeight from '@compos/use-max-height'
import usePagination from '@compos/use-pagination'
import useDecimalPrecision from '@compos/store/use-decimal-precision'

const emit = defineEmits(['update:modelValue'])
const { decimalPrecision } = useDecimalPrecision()

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
    projectId: props.detailInfo.project?.id
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
const drawerRef = ref()
const tableLoading = ref(false)
const dataFormat = computed(() => {
  return [
    ['createTime', ['parse-time', '{y}-{m}-{d}']],
    ['unitPrice', ['to-thousand', decimalPrecision.value.contract]],
    ['totalPrice', ['to-thousand', decimalPrecision.value.contract]]
  ]
})

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.warehouse-record',
    extraBox: '.el-drawer__header',
    wrapperBox: '.el-drawer__body',
    extraHeight: '5vh',
    minHeight: 300,
    navbar: false,
    clientHRepMainH: true
  },
  drawerRef
)

// 获取入库记录
async function fetchList() {
  let _list = []
  tableLoading.value = true
  try {
    const { content = [], totalElements } = await warehouseRecord({ ...params.value, ...queryPage })
    content.forEach((row) => {
      row.totalPrice = row.unitPrice * row.mete
      if (row.boolReturn && row.totalPrice > 0) {
        row.totalPrice = row.totalPrice * -1
      }
    })
    _list = content
    setTotalPage(totalElements)
  } catch (error) {
    console.log('获取收款记录失败', error)
  } finally {
    list.value = _list
    tableLoading.value = false
  }
}
</script>
