<template>
  <common-dialog
    append-to-body
    v-model="visible"
    top="10vh"
    width="600px"
    :before-close="handleClose"
    title="取消送货"
    :center="false"
    :close-on-click-modal="false"
  >
    <template #titleRight>
      <common-button type="primary" size="small" @click="onSubmit">提交</common-button>
    </template>
    <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="100px">
      <el-divider><span class="title">运费信息</span></el-divider>
      <el-row>
        <el-col :span="10">
          <el-form-item label="车型">
            <span>{{ detailInfo?.logisticsDTO?.carModel }}</span>
          </el-form-item>
        </el-col>
        <el-col :span="10">
          <el-form-item label="计价方式">
            <span v-empty-text>{{ logisticsPriceTypeEnum.VL[detailInfo?.logisticsDTO?.priceType] }}</span>
          </el-form-item>
        </el-col>
      </el-row>
      <el-row>
        <el-col :span="10">
          <el-form-item label="装载重量(t)">
            <span>{{ detailInfo?.logisticsDTO?.actualWeight? detailInfo.logisticsDTO.actualWeight/1000: '-' }}</span>
          </el-form-item>
        </el-col>
        <el-col :span="10">
          <el-form-item label="运输单价">
            <span style="margin-right:3px;">{{ toFixed(detailInfo?.logisticsDTO?.price, decimalPrecision.bridge) }}</span>
            <span :class="detailInfo?.logisticsDTO?.priceType === logisticsPriceTypeEnum.WEIGHT.V ? 'blue':'orange'" >{{ detailInfo?.logisticsDTO?.priceType?logisticsPriceTypeEnum.V[detailInfo?.logisticsDTO?.priceType].unit:'' }}</span>
          </el-form-item>
        </el-col>
      </el-row>
      <el-form-item label="运输费">
        <span><span style="margin-right:3px;">{{ toFixed(detailInfo?.logisticsDTO?.totalPrice, decimalPrecision.bridge)}}</span>元</span>
      </el-form-item>
      <el-divider><span class="title">运费变更</span></el-divider>
      <el-form-item label="运费变更" prop="changeFreight">
        <common-radio v-model="form.changeFreight" :options="freightChangeTypeEnum.ENUM" type="enum" @change="typeChange"/>
      </el-form-item>
      <template v-if="form.changeFreight===freightChangeTypeEnum.CHANGE.V">
        <el-form-item label="计价方式" prop="priceType">
          <common-radio-button
            class="filter-item"
            v-model="form.priceType"
            :options="logisticsPriceTypeEnum.ENUM"
            type="enum"
            size="small"
            @change="priceChange"
          />
        </el-form-item>
        <el-form-item :label="form.priceType === logisticsPriceTypeEnum.WEIGHT.V?'运输单价':'运输费'" prop="price">
          <el-input-number
            v-model="form.price"
            :max="999999999999"
            :precision="decimalPrecision.bridge"
            :step="100"
            :controls="false"
            style="width: 150px;margin-right:3px;"
            placeholder="请填写"
            autocomplete="off"
            @change="priceChange"
          />
          <span :class="form.priceType === logisticsPriceTypeEnum.WEIGHT.V ? 'blue':'orange'" style="margin-left:3px;">{{ form.priceType?logisticsPriceTypeEnum.V[form.priceType].unit:'' }}</span>
        </el-form-item>
        <el-form-item label="运输费" v-if="form.priceType === logisticsPriceTypeEnum.WEIGHT.V">
          <span><span style="margin-right:3px;">{{ toFixed(allPrice, decimalPrecision.bridge) }}</span>元</span>
        </el-form-item>
      </template>
      <el-form-item label="变更原因" v-if="form.changeFreight && form.changeFreight!==freightChangeTypeEnum.CONTINUE.V" prop="changeFreightReason">
        <el-input
          v-model.trim="form.changeFreightReason"
          type="textarea"
          :autosize="{ minRows: 2, maxRows: 8}"
          placeholder="运费变更原因"
          style="width: 280px;"
          :maxlength="200"
          show-word-limit
        />
      </el-form-item>
      <el-form-item label="">
        <el-input
          v-model.trim="form.cancelDeliveryReason"
          type="textarea"
          :autosize="{ minRows: 3, maxRows: 8}"
          placeholder="取消送货原因"
          style="width: 280px;"
          :maxlength="500"
          show-word-limit
        />
      </el-form-item>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { deliveryCancel } from '@/api/bridge/bridge-pack-and-ship/receipt-status'
import { ref, defineProps, watch, defineEmits, nextTick } from 'vue'

import { logisticsPriceTypeEnum, freightChangeTypeEnum } from '@enum-ms/mes'
import useVisible from '@compos/use-visible'
import { toFixed } from '@/utils/data-type'
import { ElNotification } from 'element-plus'
import useWatchFormValidate from '@compos/form/use-watch-form-validate'
import useDecimalPrecision from '@compos/store/use-decimal-precision'

const { decimalPrecision } = useDecimalPrecision()

const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  },
  showType: {
    type: String,
    default: undefined
  },
  detailInfo: {
    type: Object,
    default: () => {}
  }
})

const defaultForm = {
  id: undefined,
  changeFreight: undefined,
  priceType: undefined,
  price: undefined,
  changeFreightReason: undefined,
  cancelDeliveryReason: undefined
}

const form = ref(JSON.parse(JSON.stringify(defaultForm)))
const allPrice = ref()
const formRef = ref()

const rules = {
  changeFreight: { required: true, message: '请选择运费变更类型', trigger: 'change' },
  priceType: { required: true, message: '请选择计价方式', trigger: 'change' },
  price: { required: true, message: '请填写价格', trigger: 'blur' }
}
const emit = defineEmits(['success', 'update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })

watch(
  () => visible.value,
  (val) => {
    if (val) {
      if (!props.auditStatus) {
        resetForm()
      }
    }
  },
  { deep: true, immediate: true }
)

function resetForm(data) {
  if (formRef.value) {
    formRef.value.resetFields()
  }
  form.value = JSON.parse(JSON.stringify(defaultForm))
  form.value.id = props.detailInfo.id
  form.value.changeFreight = freightChangeTypeEnum.CONTINUE.V
  if (formRef.value) {
    nextTick(() => {
      formRef.value.clearValidate()
    })
  }
  useWatchFormValidate(formRef, form)
}

function typeChange(val) {
  if (val === freightChangeTypeEnum.CHANGE.V) {
    form.value.priceType = props.detailInfo.logisticsDTO?.priceType
    form.value.price = props.detailInfo.logisticsDTO?.price
    priceChange()
  } else {
    form.value.priceType = undefined
    form.value.price = undefined
    priceChange()
  }
}

function priceChange() {
  allPrice.value = form.value.priceType === logisticsPriceTypeEnum.WEIGHT.V ? (props.detailInfo.logisticsDTO.actualWeight * form.value.price) / 1000 : undefined
}

async function onSubmit() {
  try {
    const valid = await formRef.value.validate()
    if (!valid) {
      return
    }
    await deliveryCancel(form.value)
    ElNotification({ title: '取消送货成功', type: 'success' })
    emit('success')
    handleClose()
  } catch (error) {
    console.log('取消送货失败', error)
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
