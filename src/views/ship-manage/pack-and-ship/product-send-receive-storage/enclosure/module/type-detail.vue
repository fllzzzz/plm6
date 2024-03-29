<template>
  <common-drawer
    ref="drawerRef"
    :title="
      showType === 'INBOUND'
        ? '入库明细'
        : showType === 'OUTBOUND'
        ? '出库明细'
        : showType === 'STOCK'
        ? '期末库存明细'
        : showType === 'BEGINNING'
        ? '期初库存明细'
        : '清单明细'
    "
    :close-on-click-modal="false"
    v-model="visible"
    direction="rtl"
    :before-close="handleClose"
    custom-class="product-detail"
    size="80%"
  >
    <template #titleAfter>
      <el-tag size="medium">{{ `项目：${projectNameFormatter(detailInfo.project)}` }}</el-tag>
    </template>
    <template #content>
      <div
        class="header-div"
        :style="showType === 'INBOUND' || showType === 'OUTBOUND' ? 'display: flex; justify-content: space-between' : ''"
      >
        <el-date-picker
          v-model="query.dateTime"
          type="month"
          size="small"
          class="date-item filter-item"
          placeholder="选择月"
          format="YYYY-MM"
          value-format="x"
          style="width: 120px; margin-bottom: 10px"
          v-if="showType === 'INBOUND' || showType === 'OUTBOUND'"
          @change="fetchList"
        />
        <div style="width: 300px; margin-bottom: 10px">
          <print-table
            v-permission="permission.detailPrint"
            :api-key="
              showType === 'INBOUND'
                ? 'enclosureInboundInventoryDetail'
                : showType === 'OUTBOUND'
                ? 'enclosureOutboundInventoryDetail'
                : showType === 'STOCK'
                ? 'enclosureEndInventoryDetail'
                : 'enclosureBeginningInventoryDetail'
            "
            :params="{
              projectId: props.detailQuery?.projectId,
              workshopId: props.workshopId,
              productType: props.productType,
              category: props.category,
              ...query,
              type: productSearchTypeEnum[props.showType].V,
            }"
            size="mini"
            type="warning"
            class="filter-item"
          />
        </div>
      </div>
      <common-table
        :data="list"
        v-loading="tableLoading"
        :data-format="dataFormat"
        show-summary
        :summary-method="getSummaries"
        :max-height="maxHeight - 100"
      >
        <el-table-column label="序号" type="index" align="center" width="60" />
        <!-- <el-table-column key="monomer.name" prop="monomer.name" label="单体" align="center" :show-overflow-tooltip="true">
          <template #default="{ row }">
            <table-cell-tag :show="!!row.typeName" :name="row.typeName" :color="row.typeName === '构件' ? '#67C23A' : '#409EFF'" />
            <span>{{ row.monomer ? row.monomer?.name : '-' }}</span>
          </template>
        </el-table-column>
        <el-table-column key="area.name" prop="area.name" label="区域" align="center" :show-overflow-tooltip="true" />
        <el-table-column key="serialNumber" prop="serialNumber" label="编号" align="center" :show-overflow-tooltip="true" />
        <el-table-column key="specification" prop="specification" label="规格" align="center" :show-overflow-tooltip="true" />
        <el-table-column key="length" prop="length" label="长度(mm)" align="center" :show-overflow-tooltip="true" />
        <el-table-column key="material" prop="material" label="材质" align="center" :show-overflow-tooltip="true" width="80px" />
        <el-table-column key="quantity" prop="quantity" label="数量" align="center" :show-overflow-tooltip="true" width="60px" />
        <el-table-column key="netWeight" prop="netWeight" label="单净重（kg）" align="center" :show-overflow-tooltip="true" />
        <el-table-column key="grossWeight" prop="grossWeight" label="单毛重（kg）" align="center" :show-overflow-tooltip="true" />
        <el-table-column key="totalNetWeight" prop="totalNetWeight" label="总净重（kg）" align="center" :show-overflow-tooltip="true" />
        <el-table-column key="totalGrossWeight" prop="totalGrossWeight" label="总毛重（kg）" align="center" :show-overflow-tooltip="true" /> -->
        <el-table-column key="name" prop="name" label="名称" align="center" :show-overflow-tooltip="true" />
        <el-table-column key="serialNumber" prop="serialNumber" label="编号" align="center" :show-overflow-tooltip="true" />
        <el-table-column
          v-if="props.category !== enclosureTypeEnum.FOLDING_PIECE.V"
          key="plate"
          prop="plate"
          label="板型"
          align="center"
          :show-overflow-tooltip="true"
        />
        <el-table-column key="length" prop="length" label="单长（mm）" align="center" :show-overflow-tooltip="true" />
        <el-table-column
          key="quantity"
          prop="quantity"
          :label="`${
            showType === 'INBOUND'
              ? '入库'
              : showType === 'OUTBOUND'
              ? '出库'
              : showType === 'STOCK'
              ? '期末'
              : showType === 'BEGINNING'
              ? '期初'
              : '清单'
          }数（件）`"
          align="center"
          :show-overflow-tooltip="true"
        />
        <el-table-column
          key="totalLength"
          prop="totalLength"
          :label="`${
            showType === 'INBOUND'
              ? '入库'
              : showType === 'OUTBOUND'
              ? '出库'
              : showType === 'STOCK'
              ? '期末'
              : showType === 'BEGINNING'
              ? '期初'
              : '清单'
          }量（m）`"
          align="center"
          :show-overflow-tooltip="true"
        >
          <template #default="{ row }">
            <span>{{ convertUnits(row.totalLength, 'mm', 'm', DP.MES_ENCLOSURE_L__M) || 0 }}</span>
          </template>
        </el-table-column>
        <el-table-column
          key="createTime"
          prop="createTime"
          :label="showType === 'INBOUND' ? '入库日期' : '出库日期'"
          align="center"
          :show-overflow-tooltip="true"
          v-if="showType === 'INBOUND' || showType === 'OUTBOUND'"
        />
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
import { enclosureProductDetail } from '@/api/ship-manage/pack-and-ship/enclosure-product-receive-send-storage'
import { ref, defineEmits, defineProps, watch } from 'vue'
import { convertUnits } from '@/utils/convert/unit'
import { enclosureTypeEnum } from '@enum-ms/ship-manage'
// import { tableSummary } from '@/utils/el-extra'
import { DP } from '@/settings/config'
import { projectNameFormatter } from '@/utils/project'
import { productSearchTypeEnum } from '@enum-ms/mes'

import useVisible from '@compos/use-visible'
import useMaxHeight from '@compos/use-max-height'
import usePagination from '@compos/use-pagination'

const emit = defineEmits(['update:modelValue', 'success'])
const query = ref({})

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
  },
  showType: {
    type: String,
    default: undefined
  },
  detailQuery: {
    type: Object,
    default: () => {}
  },
  workshopId: {
    type: Number
  },
  productType: {
    type: Number
  },
  category: {
    type: Number
  }
})

const { visible, handleClose } = useVisible({ emit, props })
const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetchList })

watch(visible, (val) => {
  if (val) {
    // query.value.createTime = []
    // query.value.startDate = undefined
    // query.value.endDate = undefined
    query.value.dateTime = props.detailQuery?.dateTime
    fetchList()
  }
})

const list = ref([])
const drawerRef = ref()
const tableLoading = ref(false)

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.product-detail',
    extraBox: ['.el-drawer__header', '.header-div'],
    wrapperBox: '.el-drawer__body',
    paginate: true,
    minHeight: 300,
    navbar: false,
    clientHRepMainH: true
  },
  drawerRef
)

const dataFormat = ref([['createTime', 'parse-time']])

// 合计
function getSummaries(param) {
  const { columns, data } = param
  const sums = []
  columns.forEach((column, index) => {
    if (index === 0) {
      sums[index] = '合计'
      return
    }
    if (column.property === 'quantity') {
      const values = data.map((item) => Number(item[column.property]))
      let valuesSum = 0
      if (!values.every((value) => isNaN(value))) {
        valuesSum = values.reduce((prev, curr) => {
          const value = Number(curr)
          if (!isNaN(value)) {
            return prev + curr
          } else {
            return prev
          }
        }, 0)
      }
      sums[index] = valuesSum
    }
    if (column.property === 'totalLength') {
      const values = data.map((item) => Number(item[column.property]))
      let valuesSum = 0
      if (!values.every((value) => isNaN(value))) {
        valuesSum = values.reduce((prev, curr) => {
          const value = Number(curr)
          if (!isNaN(value)) {
            return prev + curr
          } else {
            return prev
          }
        }, 0)
      }
      sums[index] = convertUnits(valuesSum, 'mm', 'm', DP.MES_ENCLOSURE_L__M)
    }
  })
  return sums
}

// function timeChange(val) {
//   if (val && val.length) {
//     query.value.startDate = val[0]
//     query.value.endDate = val[1]
//   } else {
//     query.value.startDate = undefined
//     query.value.endDate = undefined
//   }
//   fetchList()
// }
// 获取明细
async function fetchList() {
  let _list = []
  tableLoading.value = true
  try {
    const { content = [], totalElements } = await enclosureProductDetail({
      projectId: props.detailQuery?.projectId,
      workshopId: props.workshopId,
      productType: props.productType,
      category: props.category,
      ...queryPage,
      ...query.value,
      type: productSearchTypeEnum[props.showType].V
    })
    _list = content
    setTotalPage(totalElements)
  } catch (error) {
    console.log('明细', error)
  } finally {
    list.value = _list
    tableLoading.value = false
  }
}
</script>
