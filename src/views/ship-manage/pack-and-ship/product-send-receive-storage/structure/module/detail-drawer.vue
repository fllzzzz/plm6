<template>
  <common-drawer
    :before-close="handleClose"
    v-model="showDrawer"
    size="90%"
    :title="
      props.inventoryType === 1 ? '期初库存量' : props.inventoryType === 2 ? '入库量' : props.inventoryType === 3 ? '出库量' :  props.inventoryType === 4 ? '期末库存量' : '库存价值'
    "
  >
    <template #titleRight>
      <print-table
        :api-key="props.inventoryType === 1 ? 'structureFinishedStartDetail' : props.inventoryType === 2 ? 'structureFinishedInDetail' : props.inventoryType === 3 ? 'structureFinishedOutDetail' :  props.inventoryType === 4 ? 'structureFinishedEndtDetail' : 'structureTotalPriceDetail'"
        v-permission="permission.detailPrint"
        :params="{
          ...queryList,
          ...updata
           }"
      ></print-table>
    </template>
    <template #content>
      <div class="head-container">
        <div class="header-div">
          <project-cascader class="filter-item" v-model="queryList.projectId" @change="fetchList" clearable />
          <monomer-select
            class="filter-item"
            ref="monomerSelectRef"
            v-model="updata.monomerId"
            :project-id="queryList.projectId"
            clearable
            :default="false"
            @change="fetchList"
            @getAreaInfo="getAreaInfo"
          />
          <common-select
            class="filter-item"
            placeholder="请选择区域"
            v-model="updata.areaId"
            :options="areaInfo"
            :dataStructure="{ key: 'id', label: 'name', value: 'id' }"
            clearable
            @change="fetchList"
          />
          <el-input
            class="filter-item"
            style="width: 200px"
            placeholder="编号搜索"
            v-model.trim="updata.serialNumber"
            @keyup.enter="fetchList"
          />
          <common-button class="filter-item" size="mini" type="success" icon="el-icon-search" @click="fetchList" style="margin-left: 10px">
            搜索
          </common-button>
          <common-button class="filter-item" size="mini" type="warning" icon="el-icon-refresh-left" @click=";(updata = {}), fetchList()">
            重置
          </common-button>
        </div>
        <common-table :data="list" v-loading="tableLoading" :data-format="dataFormat">
          <el-table-column label="序号" align="center" type="index" min-width="50" />
          <el-table-column label="项目简称" align="center" min-width="120" prop="project">
            <template #default="{ row }">
              <span>{{ row.project?.serialNumber || '' }}-{{ row.project?.shortName || '' }}</span>
            </template>
          </el-table-column>
          <el-table-column label="单体" align="center" min-width="100" prop="monomer.name">
            <template #default="{row}">
              <table-cell-tag v-if="row.boolAllPartSendDirectly && row.boolShowSign" name="檩条直发" color="#f56c6c" />
              <table-cell-tag v-if="!row.boolAllPartSendDirectly && row.boolShowSign" name="直发件" />
              <span>{{ row.monomer?.name }}</span>
            </template>
          </el-table-column>
          <el-table-column label="区域" align="center" min-width="100" prop="area.name" />
          <el-table-column label="部门" align="center" min-width="100" prop="workshopName" />
          <el-table-column label="编号" align="center" min-width="100" prop="serialNumber" />
          <el-table-column label="规格" align="center" min-width="100" prop="specification" />
          <el-table-column label="长度(mm)" align="center" min-width="80" prop="length" />
          <el-table-column label="材质" align="center" min-width="80" prop="material" />
          <el-table-column label="数量" align="center" min-width="50" prop="quantity" />
          <el-table-column label="单净重(kg)" align="center" min-width="80" prop="netWeight" />
          <el-table-column label="单毛重(kg)" align="center" min-width="80" prop="grossWeight" />
          <el-table-column label="总净重(kg)" align="center" min-width="80" prop="totalNetWeight" />
          <el-table-column label="总毛重(kg)" align="center" min-width="80" prop="totalGrossWeight" />
          <el-table-column v-if="props.inventoryType === 5" label="价值(元)" align="center" min-width="80" prop="totalPrice" />
        </common-table>
        <!-- 分页 -->
        <el-pagination
          :total="total"
          :current-page="queryPage.pageNumber"
          :page-size="queryPage.pageSize"
          @size-change="handleSizeChange"
          @current-change="handleCurrentChange"
          layout="total, prev, pager, next, sizes"
          style="margin-top: 8px"
        />
      </div>
    </template>
  </common-drawer>
</template>
<script setup>
import { defineProps, defineEmits, computed, ref } from 'vue'
import { mesProductSendReceiveStoragePM as permission } from '@/page-permission/ship-manage'
import { artifactProductDetail } from '@/api/ship-manage/pack-and-ship/product-receive-send-storage'
import { DP } from '@/settings/config'
import usePagination from '@compos/use-pagination'
import useVisible from '@compos/use-visible'
import monomerSelect from '@/components-system/plan/monomer-select'
import projectCascader from '@comp-base/project-cascader'

const props = defineProps({
  showDetailDrawer: {
    type: Boolean
  },
  query: {
    type: Object,
    default: () => {}
  },
  inventoryType: {
    type: Number
  },
  workshopId: {
    type: Number
  }
})

const emit = defineEmits(['update:showDetailDrawer'])

const { visible: showDrawer, handleClose } = useVisible({
  emit,
  props,
  field: 'showDetailDrawer',
  showHook: fetchList
})

const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetchList })

const dataFormat = ref([
  ['netWeight', ['to-fixed', DP.COM_WT__KG]],
  ['grossWeight', ['to-fixed', DP.COM_WT__KG]],
  ['totalNetWeight', ['to-fixed', DP.COM_WT__KG]],
  ['totalGrossWeight', ['to-fixed', DP.COM_WT__KG]],
  ['totalPrice', ['to-fixed', DP.YUAN]]
])

const list = ref([])
const tableLoading = ref(false)
const areaInfo = ref([])
const updata = ref({
  monomerId: undefined,
  areaId: undefined,
  serialNumber: undefined
})

const queryList = computed(() => {
  return {
    projectId: props.query.projectId,
    productType: props.query.productType,
    dateTime: props.query.dateTime,
    workshopId: props.workshopId,
    type: props.inventoryType
  }
})

function getAreaInfo(val) {
  areaInfo.value = val || []
}

async function fetchList() {
  list.value = []
  tableLoading.value = true
  try {
    const res = await artifactProductDetail({
      ...queryList.value,
      ...updata.value,
      ...queryPage
    })
    list.value = res.content
    setTotalPage(res.totalElements)
    console.log(res)
  } catch (error) {
    console.log(error)
  } finally {
    tableLoading.value = false
  }
}
</script>
