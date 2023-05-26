<template>
  <common-drawer
    ref="drawerRef"
    title="详情"
    :close-on-click-modal="false"
    v-model="visible"
    direction="rtl"
    :before-close="handleClose"
    custom-class="type-detail"
    size="90%"
  >
    <template #titleAfter>
      <el-tag size="medium">{{ `项目：${projectNameFormatter(detailInfo.project)}` }}</el-tag>
      <el-tag size="medium">{{ `查询日期：${dateTime ? parseTime(dateTime, '{y}-{m}') : '-'}` }}</el-tag>
    </template>
    <template #titleRight>
      <print-table
        api-key="enclosureProductSendReceiveStorageDetail"
        v-permission="permission.detailPrint"
        :params="{ ...props.detailQuery, ...query, category: props.category, workshopId: props.workshopId, productType: props.productType }"
        size="mini"
        type="warning"
        class="filter-item"
      />
    </template>
    <template #content>
      <div class="header-div">
        <!-- <monomer-select
          ref="monomerSelectRef"
          v-model="query.monomerId"
          :project-id="props.detailInfo.project.id"
          :default="false"
          clearable
          class="filter-item"
          @change="fetchList"
          @getAreaInfo="getAreaInfo"
        />
        <common-select
          v-model="query.areaId"
          :options="areaInfo"
          type="other"
          :dataStructure="{ key: 'id', label: 'name', value: 'id' }"
          size="small"
          clearable
          placeholder="请选择区域"
          class="filter-item"
          style="width: 200px; margin-left: 3px"
          @change="fetchList"
        /> -->
        <common-select
          v-model="query.enclosurePlanId"
          :options="areaInfo"
          type="other"
          :dataStructure="{ key: 'id', label: 'name', value: 'id' }"
          size="small"
          clearable
          placeholder="请选择批次"
          class="filter-item"
          style="width: 200px; margin-left: 3px"
          @change="fetchList"
        />
        <el-input
          v-model.trim="query.serialNumber"
          size="small"
          placeholder="编号搜索"
          style="width: 200px; margin-bottom: 10px; margin-left: 3px"
          class="filter-item"
          clearable
          @keyup.enter="fetchList"
        />
        <common-button class="filter-item" size="mini" type="success" icon="el-icon-search" @click="fetchList" style="margin-left: 10px">
          搜索
        </common-button>
        <common-button class="filter-item" size="mini" type="warning" icon="el-icon-refresh-left" @click=";(query = {}), fetchList()">
          重置
        </common-button>
      </div>
      <common-table
        :data="list"
        v-loading="tableLoading"
        show-summary
        :summary-method="getSummaries"
        :max-height="maxHeight"
        v-if="visible"
      >
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column key="enclosurePlanName" prop="enclosurePlanName" label="批次" align="center" show-overflow-tooltip />
        <el-table-column key="serialNumber" prop="serialNumber" label="编号" align="center" show-overflow-tooltip />
        <el-table-column
          v-if="props.category !== enclosureTypeEnum.FOLDING_PIECE.V"
          key="plate"
          prop="plate"
          label="板型"
          align="center"
          show-overflow-tooltip
        />
        <el-table-column key="length" prop="length" label="单长(mm)" align="center" show-overflow-tooltip />
        <el-table-column label="清单数(件/米)" align="center">
          <el-table-column key="quantity" prop="quantity" label="清单数" align="center" show-overflow-tooltip />
          <el-table-column key="totalLength" prop="totalLength" label="总长" align="center" show-overflow-tooltip>
            <template #default="{ row }">
              <span>{{ convertUnits(row.totalLength, 'mm', 'm', DP.MES_ENCLOSURE_L__M) || 0 }}</span>
            </template>
          </el-table-column>
        </el-table-column>
        <el-table-column label="期初库存(件/米)" align="center">
          <el-table-column key="beginningQuantity" prop="beginningQuantity" label="期初数" align="center" show-overflow-tooltip />
          <el-table-column key="beginningTotalLength" prop="beginningTotalLength" label="总长" align="center" show-overflow-tooltip>
            <template #default="{ row }">
              <span>{{ convertUnits(row.beginningTotalLength, 'mm', 'm', DP.MES_ENCLOSURE_L__M) || 0 }}</span>
            </template>
          </el-table-column>
        </el-table-column>
        <el-table-column label="入库(件/米)" align="center">
          <el-table-column key="inboundQuantity" prop="inboundQuantity" label="入库数" align="center" show-overflow-tooltip />
          <el-table-column key="inboundTotalLength" prop="inboundTotalLength" label="总长" align="center" show-overflow-tooltip>
            <template #default="{ row }">
              <span>{{ convertUnits(row.inboundTotalLength, 'mm', 'm', DP.MES_ENCLOSURE_L__M) || 0 }}</span>
            </template>
          </el-table-column>
        </el-table-column>
        <el-table-column label="出库(件/米)" align="center">
          <el-table-column key="outboundQuantity" prop="outboundQuantity" label="出库数" align="center" show-overflow-tooltip />
          <el-table-column key="outboundTotalLength" prop="outboundTotalLength" label="总长" align="center" show-overflow-tooltip>
            <template #default="{ row }">
              <span>{{ convertUnits(row.outboundTotalLength, 'mm', 'm', DP.MES_ENCLOSURE_L__M) || 0 }}</span>
            </template>
          </el-table-column>
        </el-table-column>
        <el-table-column label="期末库存(件/米)" align="center">
          <el-table-column key="stockQuantity" prop="stockQuantity" label="期末数" align="center" show-overflow-tooltip />
          <el-table-column key="stockTotalLength" prop="stockTotalLength" label="期末量" align="center" show-overflow-tooltip>
            <template #default="{ row }">
              <span>{{ convertUnits(row.stockTotalLength, 'mm', 'm', DP.MES_ENCLOSURE_L__M) || 0 }}</span>
            </template>
          </el-table-column>
        </el-table-column>
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
import { detail } from '@/api/ship-manage/pack-and-ship/enclosure-product-receive-send-storage'
import { getEnclosureBatch } from '@/api/mes/common.js'
import { ref, defineEmits, defineProps, watch } from 'vue'
// import { tableSummary } from '@/utils/el-extra'
import { enclosureTypeEnum } from '@enum-ms/ship-manage'
import { convertUnits } from '@/utils/convert/unit'
import { DP } from '@/settings/config'
import { projectNameFormatter } from '@/utils/project'
import { parseTime } from '@/utils/date'
import useVisible from '@compos/use-visible'
import useMaxHeight from '@compos/use-max-height'
import usePagination from '@compos/use-pagination'
// import monomerSelect from '@/components-system/plan/monomer-select'

const emit = defineEmits(['update:modelValue', 'success'])
const query = ref({
  enclosurePlanId: undefined,
  serialNumber: undefined
})

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
  dateTime: {
    type: String
  },
  category: {
    type: Number
  }
})

const { visible, handleClose } = useVisible({ emit, props })
const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetchList })

watch(visible, (val) => {
  if (val) {
    query.value.areaId = undefined
    fetchBatch()
    fetchList()
  }
})

const list = ref([])
const areaInfo = ref([])
const drawerRef = ref()
const tableLoading = ref(false)

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.type-detail',
    extraBox: ['.el-drawer__header', '.header-div'],
    wrapperBox: '.el-drawer__body',
    paginate: true,
    minHeight: 300,
    navbar: false,
    clientHRepMainH: true
  },
  visible
)

// 合计
function getSummaries(param) {
  // const summary = tableSummary(param, {
  //   props: [
  //     'inboundQuantity',
  //     'inboundTotalLength',
  //     'outboundQuantity',
  //     'outboundTotalLength',
  //     'quantity',
  //     'totalLength',
  //     'stockTotalLength',
  //     'stockQuantity',
  //     'beginningQuantity',
  //     'beginningTotalLength'
  //   ]
  // })
  // return summary
  const { columns, data } = param
  const sums = []
  columns.forEach((column, index) => {
    if (index === 0) {
      sums[index] = '合计'
      return
    }
    if (
      column.property === 'quantity' ||
      column.property === 'inboundQuantity' ||
      column.property === 'outboundQuantity' ||
      column.property === 'stockQuantity' ||
      column.property === 'beginningQuantity'
    ) {
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
    if (
      column.property === 'totalLength' ||
      column.property === 'inboundTotalLength' ||
      column.property === 'outboundTotalLength' ||
      column.property === 'stockTotalLength' ||
      column.property === 'beginningTotalLength'
    ) {
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

// function getAreaInfo(val) {
//   areaInfo.value = val || []
// }

// 获取围护批次
async function fetchBatch() {
  try {
    const data = await getEnclosureBatch(props.detailQuery?.projectId)
    areaInfo.value = data || []
    areaInfo.value = areaInfo.value.filter(v => v.category === props.category)
  } catch (e) {
    console.log('获取围护的批次失败', e)
  }
}

// 获取明细
async function fetchList() {
  let _list = []
  tableLoading.value = true
  try {
    const { content = [], totalElements } = await detail({
      workshopId: props.workshopId,
      productType: props.productType,
      category: props.category,
      ...props.detailQuery,
      ...query.value,
      ...queryPage
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
