<template>
  <common-drawer
    ref="drawerRef"
    append-to-body
    v-model="visible"
    :before-close="handleClose"
    :title="showType === 'edit'?'运输费变更':'运输费历史记录'"
    :center="false"
    :close-on-click-modal="false"
    size="800px"
  >
    <template #titleRight>
      <common-button :loading="loading" type="primary" size="mini" @click="onSubmit" v-if="showType === 'edit'">确认</common-button>
    </template>
    <template #content>
      <el-form ref="formRef" size="small" :model="form" :rules="rules" label-width="110px">
        <div style="display:flex;" class="detail-header">
          <div style="flex:1;padding-right:8px;">
            <el-divider v-if="showType === 'edit'"><span class="title">当前信息</span></el-divider>
            <div :class="showType === 'detail'?'flex-div':''">
              <div :class="showType === 'detail'?'child-div':''">
                <el-form-item label="承运日期" v-if="showType === 'detail'">
                  <span v-parse-time="{ val: detailInfo.auditTime, fmt: '{y}-{m}-{d}' }" />
                </el-form-item>
                <el-form-item label="所属项目">
                  <span class="project-name">{{ projectNameFormatter(detailInfo.project) }}</span>
                </el-form-item>
                <el-form-item label="物流公司">
                  <span>{{ detailInfo.supplier && detailInfo.supplier.name }}</span>
                </el-form-item>
                <el-form-item label="车次编号">
                  <el-tag effect="light" disable-transitions style="width: 100%; max-width: 130px">{{ detailInfo.serialNumber }}</el-tag>
                </el-form-item>
                <el-form-item label="车牌号">
                  <span>{{ detailInfo.licensePlate }}</span>
                </el-form-item>
                <el-form-item label="司机姓名">
                  <span>{{ detailInfo.driverName }}</span>
                </el-form-item>
              </div>
              <div :class="showType === 'detail'?'child-div':''">
                <el-form-item label="车型">
                  <span>{{ detailInfo.carModel }}</span>
                </el-form-item>
                <el-form-item label="装载重量(t)">
                  <span>{{ convertUnits(detailInfo.mete, 'kg', 't', DP.COM_WT__T) }}</span>
                </el-form-item>
                <el-form-item label="计价方式">
                  <span>{{ logisticsPriceTypeEnum.VL[detailInfo.supplier?.priceType] }}</span>
                </el-form-item>
                <el-form-item label="运输单价">
                  <span style="margin-right:3px;">{{ toFixed(detailInfo.supplier.price, DP.YUAN) }}</span>
                  <span :class="detailInfo.supplier.priceType === logisticsPriceTypeEnum.WEIGHT.V ? 'blue':'orange'" >{{ logisticsPriceTypeEnum.V[detailInfo.supplier.priceType].unit }}</span>
                </el-form-item>
                <el-form-item label="运输费">
                  <span><span style="margin-right:3px;">{{ toFixed(detailInfo.totalPrice, DP.YUAN)}}</span>元</span>
                </el-form-item>
              </div>
            </div>
          </div>
          <div style="flex:1;padding-left:8px;" v-if="showType === 'edit'">
            <el-divider><span class="title">变更信息</span></el-divider>
            <el-form-item label="计价方式" prop="priceType">
              <common-radio-button
                class="filter-item"
                v-model="form.priceType"
                :options="logisticsPriceTypeEnum.ENUM"
                type="enum"
                size="small"
              />
            </el-form-item>
            <el-form-item :label="form.priceType === logisticsPriceTypeEnum.WEIGHT.V?'运输单价':'运输费'" prop="price">
              <el-input-number
                v-model="form.price"
                :max="999999999999"
                :precision="DP.YUAN"
                :step="100"
                :controls="false"
                style="width: 150px;margin-right:3px;"
                placeholder="请填写"
                autocomplete="off"
                @change="priceChange"
              />
              <span :class="form.priceType === logisticsPriceTypeEnum.WEIGHT.V ? 'blue':'orange'" style="margin-left:3px;">{{ logisticsPriceTypeEnum.V[form.priceType].unit }}</span>
            </el-form-item>
            <el-form-item label="运输费" v-if="form.priceType === logisticsPriceTypeEnum.WEIGHT.V">
              <span><span style="margin-right:3px;">{{ toFixed(allPrice, DP.YUAN) }}</span>元</span>
            </el-form-item>
            <el-form-item label="变更原因">
              <el-input
                v-model.trim="form.remark"
                type="textarea"
                :autosize="{ minRows: 2, maxRows: 8}"
                placeholder="请填写"
                style="width: 280px;"
                :maxlength="200"
                show-word-limit
              />
            </el-form-item>
          </div>
        </div>
        <el-divider v-if="showType === 'detail'"><span class="title">历史运费记录</span></el-divider>
         <common-table
          v-if="showType === 'detail'"
          ref="detailRef"
          border
          :data="logList"
          :max-height="maxHeight"
          style="width: 100%"
          class="table-form"
          return-source-data
        >
          <el-table-column label="序号" type="index" align="center" width="50" />
          <el-table-column prop="updateTime" label="日期" align="center">
            <template v-slot="scope">
              <span v-parse-time="{ val: scope.row.updateTime, fmt: '{y}-{m}-{d} {h}:{i}:{s}' }" />
            </template>
          </el-table-column>
          <el-table-column prop="priceType" label="计价方式" align="center">
            <template v-slot="scope">
              <span>{{ logisticsPriceTypeEnum.VL[scope.row.priceType] }}</span>
            </template>
          </el-table-column>
          <el-table-column prop="price" label="运输单价" align="right">
            <template v-slot="scope">
              <span style="margin-right:3px;">{{ toFixed(scope.row.price, DP.YUAN) }}</span>
              <span :class="scope.row.priceType === logisticsPriceTypeEnum.WEIGHT.V ? 'blue':'orange'" v-if="scope.row.price">{{ logisticsPriceTypeEnum.V[scope.row.priceType].unit }}</span>
            </template>
          </el-table-column>
          <el-table-column prop="price" label="运输费（元）" align="right">
            <template v-slot="scope">
              <span style="margin-right:3px;">{{ toFixed(scope.row.totalPrice, DP.YUAN) }}</span>
            </template>
          </el-table-column>
          <el-table-column prop="changeUserName" label="价格录入人" align="center" />
        </common-table>
      </el-form>
    </template>
  </common-drawer>
</template>

<script setup>
import crudApi, { getLog } from '@/api/mes/pack-and-ship/logistics-list'
import { defineProps, defineEmits, ref, watch, nextTick } from 'vue'
import { ElNotification } from 'element-plus'

import { DP } from '@/settings/config'
import { toFixed } from '@/utils/data-type'
import { logisticsPriceTypeEnum } from '@enum-ms/mes'
import { projectNameFormatter } from '@/utils/project'
import { convertUnits } from '@/utils/convert/unit'

import useVisible from '@compos/use-visible'
import useWatchFormValidate from '@compos/form/use-watch-form-validate'
import useMaxHeight from '@compos/use-max-height'

const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  },
  showType: {
    type: String,
    default: 'detail'
  },
  detailInfo: {
    type: Object,
    default: () => {}
  }
})

const formRef = ref()
const drawerRef = ref()
const allPrice = ref()
const loading = ref(false)
const logList = ref([])

const defaultForm = {
  id: undefined,
  price: undefined,
  priceType: undefined,
  remark: undefined
}
const form = ref(JSON.parse(JSON.stringify(defaultForm)))

const validatePrice = (rule, value, callback) => {
  if (!value) {
    callback(new Error('必填且大于0'))
  } else if (form.value.priceType === props.detailInfo.priceType && value === props.detailInfo.supplier?.price) {
    callback(new Error('价格未变动'))
  }
  callback()
}

const rules = {
  price: { required: true, validator: validatePrice, trigger: 'blur' },
  priceType: { required: true, message: '请选择计价方式', trigger: 'change' }
}

const emit = defineEmits(['success', 'update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })

const { maxHeight } = useMaxHeight(
  {
    extraBox: ['.el-drawer__header', '.detail-header'],
    wrapperBox: ['.el-drawer__body'],
    navbar: false,
    extraHeight: 120,
    minHeight: 300
  },
  () => drawerRef.value.loaded
)

watch(
  () => visible.value,
  (val) => {
    if (val) {
      if (props.showType === 'edit') {
        resetForm()
      } else {
        fetchLogData()
      }
    }
  },
  { deep: true, immediate: true }
)

async function fetchLogData() {
  try {
    logList.value = await getLog(props.detailInfo.id) || []
  } catch (error) {
    console.log('获取价格变更记录', error)
  }
}
function resetForm() {
  if (formRef.value) {
    formRef.value.resetFields()
  }
  form.value.id = props.detailInfo.id
  form.value.price = props.detailInfo.supplier?.price
  form.value.priceType = props.detailInfo.priceType
  form.value.remark = undefined
  allPrice.value = props.detailInfo.totalPrice
  if (formRef.value) {
    nextTick(() => {
      formRef.value.clearValidate()
    })
  }
  useWatchFormValidate(formRef, form)
}

function priceChange() {
  allPrice.value = form.value.priceType === logisticsPriceTypeEnum.WEIGHT.V ? (props.detailInfo.mete * form.value.price) / 1000 : undefined
}

async function onSubmit() {
  loading.value = true
  try {
    const valid = await formRef.value.validate()
    if (!valid) {
      return
    }
    await crudApi.edit(form.value.id, form.value)
    ElNotification({ title: '修改成功', type: 'success' })
    emit('success')
    handleClose()
  } catch (error) {
    console.log('价格修改失败', error)
  } finally {
    loading.value = false
  }
}

</script>

<style rel="stylesheet/scss" lang="scss" scoped>
.flex-div{
  display:flex;
}
.child-div{
  flex:1;
}
.blue{
  color:#409eff;
}
.orange{
  color:#e6a23c;
}
</style>
