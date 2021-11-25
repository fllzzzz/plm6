<template>
  <div class="inbound-application-container">
    <common-wrapper :basicClass="STEEL_ENUM">
      <div class="filter-container">
        <div class="filter-right-box">
          <common-button class="filter-item" type="success" @click="materialSelectVisible = true">添加物料</common-button>
        </div>
      </div>
      <el-form ref="formRef" :model="form" size="small" label-position="right" inline label-width="80px">
        <steel-plate />
      </el-form>
    </common-wrapper>
    <common-drawer
      ref="drawerRef"
      v-model="materialSelectVisible"
      title="物料选择"
      :show-close="true"
      :size="800"
      custom-class="material-table-spec-select"
    >
      <template #content>
        <material-table-spec-select :width="300" />
      </template>
    </common-drawer>
  </div>
</template>

<script setup>
import { ref } from 'vue'
import { STEEL_ENUM } from '@/settings/config'

import useForm from '@/composables/form/use-form'
import commonWrapper from '../components/common-wrapper.vue'
import steelPlate from './module/steel-plate.vue'
import materialTableSpecSelect from '@/components-system/classification/material-table-spec-select.vue'

const permission = ['wms_inventoryWarning:get']

const defaultForm = {
  purchaseId: null, // 申购单id
  loadingWeight: null, // 装载重量
  licensePlate: null, // 车牌号
  steelPlateList: [] // 钢板列表
}

const formRef = ref()
const materialSelectVisible = ref(false)

setTimeout(() => {
  materialSelectVisible.value = true
}, 500)

const { form } = useForm(
  {
    title: '钢材入库',
    formStore: true,
    formStoreKey: 'WMS_INBOUND_APPLICATION_STEEL',
    permission: permission,
    defaultForm: defaultForm,
    crudApi: ''
  },
  formRef
)
</script>

<style lang="scss" scoped>
.inbound-application-container {
  position: relative;
  .header {
    padding: 20px 20px 10px 20px;
  }
  .footer {
    position: absolute;
    bottom: 0;
    left: 0;
  }
  height: 100%;
}
</style>
