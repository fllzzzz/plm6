<template>
  <common-drawer v-model="visible" :before-close="handleClose" size="60%" :title="props.title">
    <template #titleRight>
      <common-button type="primary" size="mini" @click="verify">确认</common-button>
    </template>
    <template #content>
      <el-form :inline="true">
        <el-form-item label="出售日期：" style="margin-right: 50px">
          <el-date-picker class="input-underline" placeholder="请填写出售日期" v-model="form.saleDate" value-format="x" />
        </el-form-item>
        <el-form-item label="购买单位：" style="margin-right: 50px">
          <common-select
            class="input-underline"
            placeholder="请填写购买单位"
            v-model="form.contractWasteId"
            :options="buyOptions"
            @change="buyChange"
          />
        </el-form-item>
        <el-form-item label="出售单位：" style="margin-right: 50px">
          <common-select class="input-underline" placeholder="请填写出售单位" v-model="form.branchCompanyId" :options="sellOptions" />
        </el-form-item>
        <el-form-item label="合同号：" style="margin-right: 50px">
          <el-input class="input-underline" placeholder="请填写合同号" v-model="form.contractNumber" :disabled="true" />
        </el-form-item>
        <el-form-item label="车牌号：" style="margin-right: 50px">
          <el-input class="input-underline" placeholder="请填写车牌号" v-model="form.plateNumber" />
        </el-form-item>
        <el-form-item label="备注：" style="width: 800px; min-height: 55px">
          <el-input type="textarea" maxlength="200" style="width: 700px" v-model="form.creatorRemark" />
        </el-form-item>
      </el-form>
      <div style="display: flex; flex-direction: column; align-items: center">
        <common-table :data="tableData" returnSourceData>
          <el-table-column label="序号" align="center" width="55" type="index"></el-table-column>
          <el-table-column label="类型" align="center" width="150">
            <template #default="{ row }">
              <common-select
                v-model="row.wasteClassificationId"
                :options="scrapTypeList"
                :data-structure="{ key: 'id', label: 'name', value: 'id' }"
                @change="typeChange(row)"
              />
            </template>
          </el-table-column>
          <el-table-column label="核算单位" align="center" width="80">
            <template #default="{ row }">
              <span>{{ row.measureUnit }}</span>
            </template>
          </el-table-column>
          <el-table-column label="数量" align="center" width="80">
            <template #default="{ row }">
              <el-input-number v-model="row.saleMete" :controls="false" />
            </template>
          </el-table-column>
          <el-table-column label="单价" align="center" width="100">
            <template #default="{ row }">
              <el-input-number v-model="row.price" :controls="false" :disabled="priceType !== 1" />
            </template>
          </el-table-column>
          <el-table-column label="金额" align="center" width="100">
            <template #default="{ row }">
              <el-input-number v-model="row.amount" :controls="false" :disabled="priceType !== 1" />
            </template>
          </el-table-column>
          <el-table-column label="备注" align="center">
            <template #default="{ row }">
              <el-input v-model="row.remark" />
            </template>
          </el-table-column>
          <el-table-column label="操作" align="center" width="120">
            <template #default="{ $index }">
              <common-button type="danger" icon="el-icon-delete" @click="delColumn($index)" />
            </template>
          </el-table-column>
        </common-table>
        <div style="margin-top: 10px">
          <common-button type="success" icon="el-icon-plus" @click="addColumn">添加一行</common-button>
        </div>
      </div>
    </template>
  </common-drawer>
</template>
<script setup>
import { defineProps, defineEmits, ref, watch, nextTick } from 'vue'
import useVisible from '@compos/use-visible'
import { getSellUnit, getBuyUnit, addScrapSell, getPriceType, editScrapSell } from '@/api/wms/scrap-sell'
import { getScrapTypeList } from '@/api/contract/scrap-ledger'
import { ElMessage } from 'element-plus'

const props = defineProps({
  modelValue: {
    type: Boolean,
    default: false
  },
  typeList: {
    type: Array,
    default: () => []
  },
  title: {
    type: String,
    default: '创建废料出售'
  },
  rowData: {
    type: Object,
    default: () => {}
  }
})

const emit = defineEmits(['update:modelValue', 'success'])
const { visible, handleClose } = useVisible({ props, emit, showHook: fetchData })

const defaultForm = {
  branchCompanyId: undefined,
  contractWasteId: undefined,
  creatorRemark: undefined,
  id: undefined,
  plateNumber: undefined,
  saleDate: new Date().getTime().toString(),
  contractNumber: undefined,
  wasteSaleDetailList: []
}

const form = ref(JSON.parse(JSON.stringify(defaultForm)))
const tableData = ref([])
const buyOptions = ref([])
const buyList = ref([])
const sellOptions = ref([])
const priceType = ref()
const scrapTypeList = ref([])

async function fetchData() {
  if (props.title === '创建废料出售') {
    tableData.value = []
    form.value = ref(JSON.parse(JSON.stringify(defaultForm)))
  } else {
    try {
      const { content } = await getScrapTypeList()
      console.log(content)
      scrapTypeList.value = content
    } catch (error) {
      console.log(error)
    }
    console.log(props.rowData)
    tableData.value = props.rowData.wasteSaleDetailList
    nextTick(() => {
      form.value.branchCompanyId = props.rowData.branchCompanyId
      form.value.contractWasteId = props.rowData.contractWasteId
      form.value.creatorRemark = props.rowData.creatorRemark
      form.value.id = props.rowData.id
      form.value.plateNumber = props.rowData.plateNumber
      form.value.saleDate = new Date(props.rowData.saleDate).getTime().toString()
      form.value.contractNumber = props.rowData.serialNumber
    })
  }
  buyList.value = []
  try {
    priceType.value = await getPriceType()
    console.log(priceType.value)
  } catch (error) {
    console.log(error)
  }
  try {
    const { content } = await getBuyUnit()
    buyList.value = content
    buyOptions.value = content.map((v) => {
      const obj = {}
      obj.id = v.id
      obj.name = v.purchaser
      return obj
    })
  } catch (error) {
    console.log('获取购买单位失败', error)
  }
  try {
    const { content } = await getSellUnit()
    console.log(content)
    sellOptions.value = content.map((v) => {
      const obj = {}
      obj.id = v.id
      obj.name = v.name
      return obj
    })
  } catch (error) {
    console.log('获取出售单位失败', error)
  }
}

// 新增行
function addColumn() {
  tableData.value.push({
    amount: undefined,
    price: undefined,
    remark: undefined,
    saleMete: undefined,
    measureUnit: undefined,
    wasteClassificationId: undefined
  })
  tableData.value.forEach((item) => {
    rowWatch(item)
  })
}

function delColumn(index) {
  tableData.value.splice(index, 1)
}

function buyChange(val) {
  buyList.value.forEach((v) => {
    if (v.id === val) {
      form.value.contractNumber = v.serialNumber
    }
  })
}

// 修改类型展示单位
function typeChange(row) {
  props.typeList.forEach(v => {
    if (v.id === row.wasteClassificationId) {
      row.measureUnit = v.measureUnit
    }
  })
}

// 行监听计金额
function rowWatch(row) {
  watch([() => row.saleMete, () => row.price], () => {
    row.amount = row.saleMete * row.price
  })
}

async function verify() {
  const _list = []
  form.value.wasteSaleDetailList = []
  tableData.value.forEach(v => {
    form.value.wasteSaleDetailList.push(v)
  })
  _list.push(form.value)
  if (props.title === '创建废料出售') {
    try {
      await addScrapSell(_list)
      emit('success')
      ElMessage({ message: '创建成功', type: 'success' })
      handleClose()
    } catch (error) {
      console.log(error, '创建废料出售')
    }
  } else {
    try {
      await editScrapSell(_list)
      emit('success')
      ElMessage({ message: '修改成功', type: 'success' })
      handleClose()
    } catch (error) {
      console.log(error, '修改废料出售')
    }
  }
}
</script>
