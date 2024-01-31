<template>
  <common-drawer v-model="visible" :before-close="handleClose" size="70%" title="废料出售审核">
    <template #titleRight>
      <common-button type="success" size="mini" @click="pass" :disabled="props.rowData.auditStatusEnum !== 1">通过</common-button>
      <common-button type="danger" size="mini" @click="noPass" :disabled="props.rowData.auditStatusEnum !== 1">驳回</common-button>
    </template>
    <template #content>
      <!-- <el-form :inline="true">
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
      </el-form> -->
      <el-descriptions :column="2" border style="margin-bottom: 20px">
        <el-descriptions-item label="创建日期">{{ form?.createTime }}</el-descriptions-item>
        <el-descriptions-item label="创建人">{{ form.createUserName }}</el-descriptions-item>
        <el-descriptions-item label="出售单位">{{ form.branchCompanyName }}</el-descriptions-item>
        <el-descriptions-item label="购买单位">{{ form.purchaser }}</el-descriptions-item>
        <el-descriptions-item label="合同编号">{{ form.serialNumber }}</el-descriptions-item>
        <el-descriptions-item label="车牌号">{{ form.plateNumber }}</el-descriptions-item>
      </el-descriptions>
      <div style="display: flex; flex-direction: column; align-items: center">
        <common-table :data="tableData" returnSourceData>
          <el-table-column label="序号" align="center" width="55" type="index"></el-table-column>
          <el-table-column label="类型" align="center" width="150">
            <template #default="{ row }">
              <!-- <common-select
                v-model="row.wasteClassificationId"
                :options="props.typeList"
                :data-structure="{ key: 'id', label: 'name', value: 'id' }"
                @change="typeChange(row)"
              /> -->
              <span>{{ row.wasteClassificationName }}</span>
            </template>
          </el-table-column>
          <el-table-column label="核算单位" align="center" width="80">
            <template #default="{ row }">
              <span>{{ row.measureUnit }}</span>
            </template>
          </el-table-column>
          <el-table-column label="数量" align="center" width="100">
            <template #default="{ row }">
              <!-- <el-input-number v-model="row.saleMete" :controls="false" /> -->
              <span>{{ row.saleMete }}</span>
            </template>
          </el-table-column>
          <el-table-column label="单价" align="center" width="125">
            <template #default="{ row }">
              <el-input-number
                v-if="priceType !== 1 || !row.price"
                v-model="row.price"
                :controls="false"
                :step="0.01"
                :max="9999999.99"
                :precision="2"
                @blur="priceBlur(row)"
              />
              <span v-else>{{ row.price }}</span>
            </template>
          </el-table-column>
          <el-table-column label="金额" align="center" width="125">
            <template #default="{ row }">
              <el-input-number
                v-if="priceType !== 1 || !row.amount"
                v-model="row.amount"
                :controls="false"
                :step="0.01"
                :max="9999999.99"
                :precision="2"
              />
              <span v-else>{{ row.amount }}</span>
            </template>
          </el-table-column>
          <el-table-column label="备注" align="center">
            <template #default="{ row }">
              <span>{{ row.remark }}</span>
            </template>
          </el-table-column>
          <!-- <el-table-column label="操作" align="center" width="120">
            <template #default="{ $index }">
              <common-button type="danger" icon="el-icon-delete" @click="delColumn($index)" />
            </template>
          </el-table-column> -->
        </common-table>
      </div>
      <!-- <div style="margin-top: 10px">
        <common-button type="success" icon="el-icon-plus" @click="addColumn">添加一行</common-button>
      </div> -->
      <div style="margin-top: 20px; display: flex; align-items: center">
        <span style="font-size: 16px; font-weight: bold">创建备注：</span>
        <el-input style="width: 750px" type="textarea" maxlength="200" :disabled="editBoolean" v-model="creatRemark" />
      </div>
      <div style="margin-top: 20px; display: flex; align-items: center">
        <span style="font-size: 16px; font-weight: bold">审核备注：</span>
        <el-input style="width: 750px" type="textarea" maxlength="200" v-model="checkRemark" />
      </div>
    </template>
  </common-drawer>
</template>
<script setup>
import { defineProps, defineEmits, ref } from 'vue'
import useVisible from '@compos/use-visible'
import { checkScrapSell, getPriceType } from '@/api/wms/scrap-sell'

const props = defineProps({
  modelValue: {
    type: Boolean,
    default: false
  },
  rowData: {
    type: Object,
    default: () => {}
  }
})

const emit = defineEmits(['update:modelValue', 'success'])
const { visible, handleClose } = useVisible({ props, emit, showHook: fetchData })

const defaultForm = {
  branchCompanyName: undefined,
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
const priceType = ref()
const checkRemark = ref()
const creatRemark = ref()
const editBoolean = ref(true)

async function fetchData() {
  tableData.value = []
  console.log(props.rowData)
  form.value = props.rowData
  creatRemark.value = props.rowData.creatorRemark
  props.rowData.wasteSaleDetailList.forEach((v) => {
    if (v?.id) {
      tableData.value.push(v)
    }
  })
  try {
    priceType.value = await getPriceType()
    console.log(priceType.value)
  } catch (error) {
    console.log(error)
  }
}

// function addColumn() {
//   tableData.value.push({
//     amount: undefined,
//     price: undefined,
//     remark: undefined,
//     saleMete: undefined,
//     unitMete: undefined,
//     wasteClassificationId: undefined
//   })
//   tableData.value.forEach((item) => {
//     rowWatch(item)
//   })
// }

// function delColumn(index) {
//   tableData.value.splice(index, 1)
// }

// function buyChange(val) {
//   buyList.value.forEach((v) => {
//     if (v.id === val) {
//       form.value.contractNumber = v.serialNumber
//     }
//   })
// }

// function typeChange(row) {
//   // console.log(row)
//   props.typeList.forEach(v => {
//     if (v.id === row.wasteClassificationId) {
//       row.unitMete = v.measureUnit
//     }
//   })
// }

// function rowWatch(row) {
//   watch([() => row.saleMete, () => row.price], () => {
//     row.amount = row.saleMete * row.price
//   })
// }

async function pass() {
  const updata = {}
  updata.id = props.rowData.id
  updata.auditorRemark = checkRemark.value
  updata.auditStatusEnum = 2
  updata.wasteSaleDetailList = tableData.value.map((v) => {
    const obj = {}
    obj.amount = v.amount
    obj.id = v.id
    obj.price = v.price
    obj.remark = v.remark
    return obj
  })
  try {
    await checkScrapSell(updata)
    emit('success')
    handleClose()
  } catch (error) {
    console.log(error, '审核废料出售')
  }
}

function priceBlur(row) {
  // rowWatch(row)
  row.amount = row.saleMete * row.price
}

async function noPass() {
  const updata = {}
  updata.id = props.rowData.id
  updata.auditorRemark = checkRemark.value
  updata.auditStatusEnum = 4
  updata.wasteSaleDetailList = tableData.value.map((v) => {
    const obj = {}
    obj.amount = v.amount
    obj.id = v.id
    obj.price = v.price
    obj.remark = v.remark
    return obj
  })
  try {
    await checkScrapSell(updata)
    emit('success')
    handleClose()
  } catch (error) {
    console.log(error, '驳回废料出售')
  }
}
</script>
