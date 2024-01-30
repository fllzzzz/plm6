<template>
  <div class="head-container">
    <div style="display: flex; justify-content: space-between">
      <div>
        <common-radio-button
          v-model="wayValue"
          :options="queryMode"
          :data-structure="{ key: 'value', label: 'label', value: 'value' }"
          class="filter-item"
          @change="dataChange"
        />
        <el-date-picker
          v-model="queryDate"
          type="daterange"
          range-separator=":"
          size="small"
          class="filter-item"
          value-format="x"
          start-placeholder="开始日期"
          end-placeholder="结束日期"
          style="width: 240px"
          @change="handelDateChange"
        />
        <common-select
          placeholder="请选择类型"
          class="filter-item"
          :options="scrapTypeList"
          :data-structure="{ key: 'id', label: 'name', value: 'id' }"
          @change="typeChange"
          v-model="scrapType"
          clearable
        />
      </div>
      <common-button type="success" size="mini" class="filter-item" @click="scrapContract">合同配置</common-button>
    </div>
    <div style="display: flex; justify-content: space-between">
      <el-tag size="medium">累计出售金额：{{ totalAmount }}元</el-tag>
      <print-table
        v-if="wayValue !== 1"
        api-key="scrapDate"
        :params="{ startDate: startDate, endDate: endDate, wasteClassificationId: scrapType }"
        v-permission="permission.print"
      />
      <print-table
        v-else
        api-key="scrapPurchaser"
        :params="{ startDate: startDate, endDate: endDate, wasteClassificationId: scrapType }"
        v-permission="permission.print"
      />
    </div>
    <contractDrawer v-model="showScrapContract" />
  </div>
</template>
<script setup>
import { ref, defineEmits } from 'vue'
import { getScrapTypeList, getTotalAmount } from '@/api/contract/scrap-ledger'
import { scrapLedgerPM as permission } from '@/page-permission/contract'
import contractDrawer from '../contract/scrap-contract.vue'
import moment from 'moment'

const emit = defineEmits(['wayChange', 'dateChange', 'typeChange'])

const queryMode = ref([
  {
    label: '按购买方查询',
    value: 1
  },
  {
    label: '按日期查询',
    value: 2
  }
])

const wayValue = ref(1)
const scrapTypeList = ref([])
const showScrapContract = ref(false)
const totalAmount = ref()
const queryDate = ref([moment().startOf('month').valueOf(), moment().valueOf()])
const startDate = ref(moment().startOf('month').valueOf())
const endDate = ref(moment().valueOf())
const scrapType = ref()

fetchScrapType()

async function fetchScrapType() {
  try {
    const { content } = await getScrapTypeList()
    console.log(content)
    scrapTypeList.value = content
  } catch (error) {
    console.log(error)
  }
  try {
    const res = await getTotalAmount({ startDate: startDate.value, endDate: endDate.value, wasteClassificationId: scrapType.value })
    totalAmount.value = res
  } catch (error) {
    console.log(error)
  }
}

const scrapContract = () => {
  showScrapContract.value = true
}

async function dataChange(v) {
  wayValue.value = v
  emit('wayChange', wayValue.value)
}

function handelDateChange(v) {
  if (v && v.length > 1) {
    startDate.value = v[0]
    endDate.value = v[1]
  } else {
    startDate.value = undefined
    endDate.value = undefined
    queryDate.value = []
  }
  queryDate.value = v
  fetchScrapType()
  emit('dateChange', startDate.value, endDate.value)
}

function typeChange(v) {
  console.log(v)
  scrapType.value = v
  fetchScrapType()
  emit('typeChange', v)
}
</script>
