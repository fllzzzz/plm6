<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <common-radio-button
        v-model="query.productType"
        :options="deliveryInstallEnumArr"
        type="enum"
        class="filter-item"
        @change="productTypeChange"
      />
      <monomer-select
        v-if="query.productType!==installProjectTypeEnum.ENCLOSURE.V"
        ref="monomerSelectRef"
        v-model="query.monomerId"
        :project-id="query.projectId"
        :main-product-type="query.productType"
        :default="false"
        clearable
        class="filter-item"
        @change="crud.toQuery"
        @getAreaInfo="getAreaInfo"
      />
       <common-select
        v-if="query.productType!==installProjectTypeEnum.ENCLOSURE.V"
        v-model="query.areaId"
        :options="areaInfo"
        type="other"
        :dataStructure="typeProp"
        size="small"
        clearable
        placeholder="请选择区域"
        class="filter-item"
        style="width:200px;"
        @change="crud.toQuery"
      />
      <template v-if="query.productType===installProjectTypeEnum.ENCLOSURE.V">
        <common-select
          v-model="query.category"
          :options="TechnologyTypeAllEnum.ENUM"
          :unshow-options="[TechnologyTypeAllEnum.STRUCTURE.K,TechnologyTypeAllEnum.BRIDGE.K]"
          type="enum"
          size="small"
          clearable
          placeholder="请选择围护类型"
          class="filter-item"
          style="width:200px;"
          @change="categoryChange"
        />
        <common-select
          v-model="query.areaId"
          :options="areaInfo"
          type="other"
          :dataStructure="typeProp"
          size="small"
          clearable
          placeholder="请选择批次"
          class="filter-item"
          style="width:200px;"
          @change="crud.toQuery"
        />
      </template>
      <el-input
        v-model.trim="query.serialNumber"
        placeholder="编号搜索"
        class="filter-item"
        style="width: 200px"
        size="small"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <el-input
        v-model.trim="query.name"
        placeholder="名称搜索"
        class="filter-item"
        style="width: 200px"
        size="small"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <rrOperation />
      <el-row v-loading="summaryLoading" v-if="checkPermission(crud.permission.get)" :gutter="20" class="panel-group">
        <el-col :span="4" class="card-panel-col">
          <Panel name="清单总量" text-color="#626262" num-color="#1890ff" :num-arr="totalAmount.totalQuantity" is-array/>
        </el-col>
        <el-col :span="4" class="card-panel-col">
          <Panel name="已收货" text-color="#626262" num-color="#1890ff" :num-arr="totalAmount.totalReceive" :precision="0" is-array/>
        </el-col>
        <el-col :span="4" class="card-panel-col">
          <Panel name="收货率(%)" text-color="#626262" num-color="#1890ff" :end-val="totalAmount.receiveRate || 0" :precision="2" />
        </el-col>
        <el-col :span="4" class="card-panel-col">
          <Panel name="已安装" text-color="#626262" num-color="#1890ff" :num-arr="totalAmount.totalInstall" is-array />
        </el-col>
        <el-col :span="4" class="card-panel-col">
          <Panel name="安装率(%)" text-color="#626262" num-color="#1890ff" :end-val="totalAmount.installRate || 0" :precision="2" />
        </el-col>
        <el-col :span="4" class="card-panel-col">
          <Panel name="工地库存" text-color="#626262" num-color="#1890ff" :num-arr="totalAmount.extraQuantity" is-array />
        </el-col>
      </el-row>
    </div>
    <crudOperation>
       <template #viewLeft>
          <print-table
            v-permission="crud.permission.print"
            api-key="deliveryInstallList"
            :params="{ ...query, queryType: 1 }"
            size="mini"
            type="warning"
            class="filter-item"
          />
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { deliveryInstallSummary } from '@/api/project-manage/delivery-manage/delivery-report/report-list'
import { allProjectPlan } from '@/api/enclosure/enclosure-plan/area'
import { ref, watch, defineProps } from 'vue'

import { TechnologyTypeAllEnum } from '@enum-ms/contract'
import { deliveryInstallTypeEnum, installProjectTypeEnum } from '@enum-ms/project'
import checkPermission from '@/utils/system/check-permission'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
import monomerSelect from '@/components-system/plan/monomer-select'
import Panel from '../../../components/Panel'
import { mapGetters } from '@/store/lib'

const { deliveryInstallEnumArr } = mapGetters('deliveryInstallEnumArr')

const defaultQuery = {
  projectId: { value: undefined, resetAble: false },
  productType: deliveryInstallTypeEnum.ARTIFACT.V,
  monomerId: undefined,
  areaId: undefined,
  name: undefined,
  serialNumber: undefined
}
const props = defineProps({
  projectId: {
    type: [Number, String],
    default: undefined
  }
})

const { crud, query } = regHeader(defaultQuery)
const typeProp = { key: 'id', label: 'name', value: 'id' }
const areaInfo = ref([])
const totalAmount = ref({})
const summaryLoading = ref(false)
const totalArea = ref([])

watch(
  () => props.projectId,
  (val) => {
    if (val) {
      crud.query.projectId = props.projectId
      totalArea.value = []
      if (crud.query.productType === installProjectTypeEnum.ENCLOSURE.V) {
        getAllProjectPlan()
      }
      crud.toQuery()
    }
  },
  { immediate: true, deep: true }
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

function productTypeChange(val) {
  query.areaId = undefined
  if (val === installProjectTypeEnum.ENCLOSURE.V) {
    getAllProjectPlan()
  }
  crud.toQuery()
}

async function fetchSummaryInfo() {
  if (!query.projectId) {
    return
  }
  summaryLoading.value = true
  try {
    const data = await deliveryInstallSummary(query)
    totalAmount.value = {
      ...data,
      receiveRate: data.receivingQuantity ? ((data.receivingQuantity / data.quantity) * 100) : 0,
      installRate: (data.receivingQuantity && data.installQuantity) ? ((data.installQuantity / data.receivingQuantity) * 100) : 0,
      totalQuantity: [
        {
          quantity: data.quantity,
          unit: installProjectTypeEnum.V[crud.query.productType].unit,
          precision: 0
        },
        {
          quantity: data.mete,
          unit: installProjectTypeEnum.V[crud.query.productType].accountUnit,
          precision: 2
        }
      ],
      totalReceive: [
        {
          quantity: data.receivingQuantity,
          unit: installProjectTypeEnum.V[crud.query.productType].unit,
          precision: 0
        },
        {
          quantity: data.receivingMete,
          unit: installProjectTypeEnum.V[crud.query.productType].accountUnit,
          precision: 2
        }
      ],
      totalInstall: [
        {
          quantity: data.installQuantity,
          unit: installProjectTypeEnum.V[crud.query.productType].unit,
          precision: 0
        },
        {
          quantity: data.installMete,
          unit: installProjectTypeEnum.V[crud.query.productType].accountUnit,
          precision: 2
        }
      ],
      extraQuantity: [
        {
          quantity: data.receivingQuantity ? data.receivingQuantity - (data.installQuantity || 0) : 0,
          unit: installProjectTypeEnum.V[crud.query.productType].unit,
          precision: 0
        },
        {
          quantity: data.receivingMete ? data.receivingMete - (data.installMete || 0) : 0,
          unit: installProjectTypeEnum.V[crud.query.productType].accountUnit,
          precision: 2
        }
      ]
    }
  } catch (error) {
    console.log('获取汇总数据', error)
  } finally {
    summaryLoading.value = false
  }
}

function getAreaInfo(val) {
  areaInfo.value = val || []
}

function categoryChange(val) {
  areaInfo.value = totalArea.value?.filter(v => v.category === val) || []
  crud.toQuery()
}

async function getAllProjectPlan() {
  crud.query.monomerId = undefined
  areaInfo.value = []
  if (props.projectId) {
    try {
      const data = await allProjectPlan(props.projectId) || []
      totalArea.value = data || []
      if (crud.query.category) {
        areaInfo.value = totalArea.value?.filter(v => v.category === crud.query.category)
      } else {
        areaInfo.value = totalArea.value
      }
    } catch (e) {
      console.log('获取项目所有围护计划', e)
    }
  }
}
</script>
<style lang="scss" scoped>
.panel-group {
  margin-bottom:10px;
  ::v-deep(.card-panel) {
    .card-panel-description {
      .card-panel-text {
        text-align:left;
        margin-top: 2px;
      }
      .card-panel-num {
        display:block;
        font-size: 17px;
        text-align:right;
      }
    }
  }
}
</style>
