<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <el-date-picker
        v-model="query.dateTime"
        type="month"
        size="small"
        class="date-item filter-item"
        placeholder="选择月"
        format="YYYY-MM"
        value-format="x"
        @change="crud.toQuery"
        style="width: 120px"
      />
      <project-radio-button size="small" :type="'all'" v-model="query.projectId" class="filter-item" @change="projectChange" />
      <common-radio-button
        v-if="query.projectId"
        type="other"
        v-model="query.category"
        :options="categoryList"
        :data-structure="{ key: 'no', label: 'name', value: 'no' }"
        default
        placeholder="请选择围护类型"
        class="filter-item"
        @change="crud.toQuery"
      />
      <common-radio-button
        v-if="!query.projectId"
        type="enum"
        v-model="query.category"
        :options="enclosureTypeEnum.ENUM"
        default
        placeholder="请选择围护类型"
        class="filter-item"
        @change="crud.toQuery"
      />
      <!-- <common-radio-button
        v-model="query.productType"
        :options="[packTypeEnum.STRUCTURE, packTypeEnum.MACHINE_PART]"
        type="enum"
        size="small"
        class="filter-item"
        @change="crud.toQuery"
      /> -->
      <workshop-select
        v-model="query.workshopId"
        :workshop-type="workshopTypeEnum.ENCLOSURE.V"
        placeholder="请选择车间"
        clearable
        style="width: 200px"
        class="filter-item"
        @change="crud.toQuery"
      />
      <el-row v-loading="summaryLoading" v-if="checkPermission(crud.permission.get)" :gutter="24" class="panel-group">
        <el-col :span="6" class="card-panel-col">
          <Panel
            name="期初库存(米)"
            text-color="#626262"
            num-color="#1890ff"
            :end-val="totalAmount?.beginningTotalLength || 0"
            :precision="DP.MES_ENCLOSURE_L__M"
          />
        </el-col>
        <el-col :span="6" class="card-panel-col">
          <Panel
            name="入库量(米)"
            text-color="#626262"
            num-color="#1890ff"
            :endVal="totalAmount?.inboundTotalLength || 0"
            :precision="DP.MES_ENCLOSURE_L__M"
          />
        </el-col>
        <el-col :span="6" class="card-panel-col">
          <Panel
            name="出库量(米)"
            text-color="#626262"
            num-color="#1890ff"
            :endVal="totalAmount?.outboundTotalLength || 0"
            :precision="DP.MES_ENCLOSURE_L__M"
          />
        </el-col>
        <el-col :span="6" class="card-panel-col">
          <Panel
            name="期末库存(米)"
            text-color="#626262"
            num-color="#1890ff"
            :end-val="totalAmount?.stockTotalLength || 0"
            :precision="DP.MES_ENCLOSURE_L__M"
          />
        </el-col>
      </el-row>
    </div>
    <crudOperation>
      <template #viewLeft>
        <print-table
          v-permission="permission.print"
          api-key="enclosureProductSendReceiveStorage"
          :params="{ ...query }"
          size="mini"
          type="warning"
          class="filter-item"
        />
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { summaryData } from '@/api/ship-manage/pack-and-ship/enclosure-product-receive-send-storage'
import { ref, watch, defineProps } from 'vue'
import { enclosureTypeEnum, packTypeEnum } from '@enum-ms/ship-manage'
import { workshopTypeEnum } from '@enum-ms/common'
import { mapGetters } from '@/store/lib'
import { TechnologyTypeAllEnum } from '@enum-ms/contract'
import useUserProjects from '@compos/store/use-user-projects'
import checkPermission from '@/utils/system/check-permission'
import { DP } from '@/settings/config'
import workshopSelect from '@comp-mes/workshop-select'
import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import Panel from '@/components/Panel'
import moment from 'moment'

const props = defineProps({
  permission: {
    type: Object,
    default: () => {}
  }
})
const defaultTime = moment().valueOf().toString()
const categoryList = ref([])

const { globalProjectId } = mapGetters(['globalProjectId'])
const { projects } = useUserProjects()

const techOptions = [
  {
    name: '压型彩板',
    no: TechnologyTypeAllEnum.PROFILED_PLATE.V,
    alias: 'ENCLOSURE'
  },
  {
    name: '压型楼承板',
    no: TechnologyTypeAllEnum.PRESSURE_BEARING_PLATE.V,
    alias: 'ENCLOSURE'
  },
  {
    name: '桁架楼承板',
    no: TechnologyTypeAllEnum.TRUSS_FLOOR_PLATE.V,
    alias: 'ENCLOSURE'
  },
  {
    name: '夹芯板',
    no: TechnologyTypeAllEnum.SANDWICH_BOARD.V,
    alias: 'ENCLOSURE'
  },
  {
    name: '折边件',
    no: TechnologyTypeAllEnum.BENDING.V,
    alias: 'ENCLOSURE'
  }
]

const defaultQuery = {
  // productType: packTypeEnum.STRUCTURE.V,
  dateTime: defaultTime.toString(),
  projectId: undefined,
  category: enclosureTypeEnum.PRESSED_PLATE.V
}

const { crud, query } = regHeader(defaultQuery)
const totalAmount = ref({})
const summaryLoading = ref(false)

watch(
  () => globalProjectId.value,
  (val) => {
    if (val) {
      init(val)
    }
  },
  { immediate: true }
)
watch(
  query,
  (val) => {
    if (val) {
      fetchSummaryInfo()
    }
  },
  { immediate: true, deep: true }
)
watch(
  () => query.projectId,
  (val) => {
    if (val) {
      init(val)
    }
  },
  { immediate: true }
)
async function fetchSummaryInfo() {
  summaryLoading.value = true
  if (!props.permission?.get) return
  try {
    const data = await summaryData({
      ...query,
      productType: packTypeEnum.ENCLOSURE.V
    })
    totalAmount.value = data
    // totalAmount.value.intoWeight = (totalAmount.value.intoWeight / 1000).toFixed(DP.COM_WT__T)
    // totalAmount.value.outWeight = (totalAmount.value.outWeight / 1000).toFixed(DP.COM_WT__T)
    // totalAmount.value.stockWeight = (totalAmount.value.stockWeight / 1000).toFixed(DP.COM_WT__T)
  } catch (error) {
    console.log('获取汇总数据', error)
  } finally {
    summaryLoading.value = false
  }
}

function init(val) {
  categoryList.value = []
  const data = projects.value.find((v) => v.id === val)?.categorys
  techOptions.forEach((o) => {
    if (data?.findIndex((p) => p === o.no) > -1) {
      categoryList.value.push(o)
    }
  })
}

function projectChange(val) {
  query.category = categoryList.value[0]?.no
  crud.toQuery()
}
</script>
<style lang="scss" scoped>
.panel-group {
  margin-bottom: 10px;
  ::v-deep(.card-panel) {
    .card-panel-description {
      .card-panel-text {
        text-align: left;
        margin-top: 2px;
      }
      .card-panel-num {
        display: block;
        font-size: 17px;
        text-align: right;
      }
    }
  }
}
</style>
