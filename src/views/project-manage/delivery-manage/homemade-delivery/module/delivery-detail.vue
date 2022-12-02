<template>
  <common-drawer
    ref="drawerRef"
    title="详情"
    :close-on-click-modal="false"
    v-model="visible"
    direction="rtl"
    :before-close="handleClose"
    custom-class="delivery-detail"
    size="80%"
  >
    <template #titleAfter>
      <el-tag :type="isNotBlank(detailInfo.boolProblemReceiving)?(detailInfo.boolProblemReceiving?'warning':'success'):''" size="medium">{{isNotBlank(detailInfo.boolProblemReceiving)?(detailInfo.boolProblemReceiving?'问题收货':'正常收货'):'未收货'}}</el-tag>
      <el-tag size="medium">{{`车牌号：${detailInfo.licensePlate}`}}</el-tag>
      <print-table
        v-permission="permission.print"
        api-key="deliveryCargoList"
        :params="props.detailInfo?.id"
        size="mini"
        type="warning"
        class="filter-item"
      />
    </template>
    <template #titleRight>
      <template v-if="showType==='edit'">
        <common-button type="danger" size="mini" @click="passConfirm(1)">问题收货</common-button>
        <el-popconfirm
          confirm-button-text="确定"
          cancel-button-text="取消"
          icon-color="#626AEF"
          title="确定收货吗?"
          @confirm="passConfirm(0)"
        >
          <template #reference>
            <common-button type="success" size="mini">确定收货</common-button>
          </template>
        </el-popconfirm>
      </template>
    </template>
    <template #content>
      <div style="color:red;font-size:13px;margin-bottom:10px;" v-if="detailInfo.boolProblemReceiving && detailInfo.problemDesc">*{{detailInfo.problemDesc}}</div>
      <common-table :data="list" v-loading="tableLoading" show-summary :summary-method="getSummaries" :max-height="maxHeight">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column key="monomer.name" prop="monomer.name" label="单体" align="center" />
        <el-table-column key="area.name" prop="area.name" label="区域" align="center" />
        <el-table-column key="productType" prop="productType" label="类型" align="center">
          <template v-slot="scope">
            <el-tag>{{installProjectTypeEnum.V[scope.row.productType].SL}}</el-tag>
          </template>
        </el-table-column>
        <el-table-column key="name" prop="name" label="名称" align="center" />
        <el-table-column key="serialNumber" prop="serialNumber" label="编号" align="center" />
        <el-table-column key="specification" prop="specification" label="规格" align="center" />
        <!-- <el-table-column key="measureUnit" prop="measureUnit" label="计量单位" align="center" /> -->
        <el-table-column key="quantity" prop="quantity" label="数量" align="center" />
        <el-table-column key="accountingUnit" prop="accountingUnit" label="核算单位" align="center" />
        <el-table-column key="accountingMete" prop="accountingMete" label="核算量" align="center" />
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import crudApi, { deliveryProductList } from '@/api/project-manage/delivery-manage/homemade-delivery'
import { ref, defineEmits, defineProps, watch } from 'vue'

import { installProjectTypeEnum } from '@enum-ms/project'
import { tableSummary } from '@/utils/el-extra'
import { isNotBlank } from '@data-type/index'

import useVisible from '@compos/use-visible'
import useMaxHeight from '@compos/use-max-height'
import { ElMessageBox, ElNotification } from 'element-plus'

const emit = defineEmits(['update:modelValue', 'success'])

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
    default: 'detail'
  }
})

const { visible, handleClose } = useVisible({ emit, props })

watch(
  visible,
  (val) => {
    if (val) {
      fetchList()
    }
  }
)

const list = ref([])
const drawerRef = ref()
const tableLoading = ref(false)

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.delivery-detail',
    extraBox: '.el-drawer__header',
    wrapperBox: '.el-drawer__body',
    paginate: true,
    minHeight: 300,
    navbar: false,
    clientHRepMainH: true
  },
  drawerRef
)

// 合计
function getSummaries(param) {
  const summary = tableSummary(param, {
    props: ['accountingMete']
  })
  return summary
}

// 获取收货明细
async function fetchList() {
  let _list = []
  tableLoading.value = true
  try {
    const { content = [] } = await deliveryProductList(props.detailInfo?.id)
    _list = content
  } catch (error) {
    console.log('收货明细', error)
  } finally {
    list.value = _list
    tableLoading.value = false
  }
}

const inputValid = (val) => {
  if ((!val || !val.trim()) && val !== 0) {
    return '必填'
  }
  if (val.length > 200) {
    return '长度在 1 到 200 个字符'
  }
  return true
}

async function passConfirm(val) {
  try {
    const remarkValue = val === 1 ? await ElMessageBox.prompt('请输入问题描述', '', {
      confirmButtonText: '确定',
      cancelButtonText: '取消',
      inputType: 'textarea',
      inputValidator: inputValid,
      type: 'warning'
    }) : undefined
    const submitData = {
      boolProblemReceiving: val,
      cargoListId: props.detailInfo?.id,
      problemDesc: val === 1 ? remarkValue.value : undefined
    }
    await crudApi.edit(submitData)
    ElNotification({ title: '提交成功', type: 'success' })
    emit('success')
    handleClose()
  } catch (error) {
    console.log('审核', error)
  }
}
</script>
