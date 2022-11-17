<template>
  <common-drawer
    ref="drawerRef"
    :title="`${props.detailData?.workshop?.name}>${props.detailData?.productionLine?.name}钻孔详情`"
    v-model="drawerVisible"
    direction="rtl"
    :before-close="handleClose"
    size="60%"
  >
    <template #content>
      <div class="head-container">
        <common-radio-button
          v-model="orderType"
          :options="typeEnum.ENUM"
          type="enum"
          size="small"
          class="filter-item"
          @change="handleChange"
        />
      </div>
      <div v-if="orderType === typeEnum.NESTING_TASK_ORDER.V">
        <common-table
          ref="table"
          :data="drillData"
          empty-text="暂无数据"
          :max-height="maxHeight"
          style="width: 100%"
          show-summary
          :summary-method="getSummaries"
        >
          <el-table-column :show-overflow-tooltip="true" prop="index" label="序号" align="center" width="60" type="index" />
          <el-table-column :show-overflow-tooltip="true" prop="project" key="project.shortName" label="项目" min-width="120">
            <template v-slot="scope">
              <span>{{ projectNameFormatter(scope.row.project) }}</span>
            </template>
          </el-table-column>
          <el-table-column :show-overflow-tooltip="true" prop="serialNumber" key="serialNumber" label="编号" align="center" />
          <el-table-column :show-overflow-tooltip="true" prop="specification" key="specification" label="规格" align="center" />
          <el-table-column :show-overflow-tooltip="true" prop="material" key="material" label="材质" align="center" />
          <el-table-column :show-overflow-tooltip="true" prop="quantity" key="quantity" label="数量" align="center" />
          <el-table-column :show-overflow-tooltip="true" prop="weight" key="weight" label="重量（kg）" align="center" />
          <el-table-column :show-overflow-tooltip="true" prop="picturePath" key="picturePath" label="图形" align="center">
            <template v-slot="scope">
              <el-image style="width: 100%; height: 100%" :src="scope.row.picturePath" fit="cover" />
            </template>
          </el-table-column>
        </common-table>
      </div>
      <div v-if="orderType === typeEnum.SORTING_ORDER.V">
        <common-table ref="table" :data="drillSortData" empty-text="暂无数据" :max-height="maxHeight" style="width: 100%">
          <el-table-column :show-overflow-tooltip="true" prop="index" label="序号" align="center" width="60" type="index" />
          <el-table-column :show-overflow-tooltip="true" prop="picturePath" key="picturePath" label="图形" align="center">
            <template v-slot="scope">
              <el-image style="width: 100%; height: 100%" :src="scope.row.picturePath" fit="cover" />
            </template>
          </el-table-column>
          <el-table-column :show-overflow-tooltip="true" prop="serialNumber" key="serialNumber" label="编号" align="center">
            <template v-slot="scope">
              <span>{{ scope.row.serialNumber }}</span>
            </template>
          </el-table-column>
          <el-table-column
            :show-overflow-tooltip="true"
            prop="workshopLine"
            :key="item"
            v-for="item in workshopList"
            :label="`${item.workShopName}>${item.productionLineName}`"
            align="center"
          >
            <template v-slot="scope">
              <span>{{ scope.row[`quantity${item.productionLineId}`] || '0' }}</span>
            </template>
          </el-table-column>
        </common-table>
        <!-- 分页 -->
        <el-pagination
          :total="total"
          :current-page="queryPage.pageNumber"
          :page-size="queryPage.pageSize"
          style="margin-top: 8px"
          layout="total, prev, pager, next, sizes"
          @size-change="handleSizeChange"
          @current-change="handleCurrentChange"
        />
      </div>
    </template>
  </common-drawer>
</template>

<script setup>
import useVisible from '@compos/use-visible'
import useMaxHeight from '@compos/use-max-height'
import usePagination from '@compos/use-pagination'
import { sortingListEnum as typeEnum } from '@enum-ms/mes'
import { defineProps, defineEmits, ref } from 'vue'
import { tableSummary } from '@/utils/el-extra'
import { projectNameFormatter } from '@/utils/project'
import { showDrillDetail, showInfo } from '@/api/mes/work-order-manage/machine-part.js'

const emit = defineEmits(['update:visible'])
const drawerRef = ref()
const drillData = ref([]) // 钻孔工单详情数据
const drillSortData = ref([]) // 钻孔分拣单
const workshopList = ref([])
const orderType = ref(typeEnum.NESTING_TASK_ORDER.V)

const props = defineProps({
  visible: {
    type: Boolean,
    required: true
  },
  detailData: {
    type: Object,
    default: () => {}
  },
  processType: {
    type: Number
  }
})

const { maxHeight } = useMaxHeight(
  {
    wrapperBox: ['.el-drawer__body'],
    navbar: false
  },
  drawerRef
)
const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: drillDetailGet })

const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: drillingSortGet })

async function drillDetailGet() {
  try {
    const data = await showDrillDetail({ cutId: props.detailData.id, processType: props.processType })
    drillData.value = data
  } catch (error) {
    console.log('获取钻孔工单详情失败', error)
  }
}

// 钻孔分拣单
async function drillingSortGet() {
  let _list = []
  workshopList.value = []
  try {
    const { content = [], totalElements } = await showInfo({
      cutId: props.detailData.id,
      processType: props.processType,
      ...queryPage
    })
    setTotalPage(totalElements)
    content.map((v) => {
      v.list.map((m) => {
        if (workshopList.value.findIndex((k) => k.productionLineId === m.productionLineId) < 0) {
          workshopList.value.push({
            productionLineId: m.productionLineId,
            productionLineName: m.productionLineName,
            workShopId: m.workShopId,
            workShopName: m.workShopName
          })
        }
      })
      v.list.map((m) => {
        workshopList.value.map((k) => {
          if (m.productionLineId === k.productionLineId) {
            v[`quantity${k.productionLineId}`] = m.quantity
          }
        })
      })
    })
    _list = content
  } catch (error) {
    console.log('获取钻孔分拣单失败', error)
  } finally {
    drillSortData.value = _list
  }
}

function handleChange(val) {
  if (val === typeEnum.SORTING_ORDER.V) {
    drillingSortGet()
  } else {
    drillDetailGet()
  }
}

// 合计
function getSummaries(param) {
  return tableSummary(param, {
    props: ['quantity', 'weight']
  })
}
</script>

<style lang="scss" scoped>
</style>

