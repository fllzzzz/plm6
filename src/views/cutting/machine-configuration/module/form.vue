<template>
  <common-dialog
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > 0"
    :title="crud.status.title"
    :show-close="false"
    width="30%"
    top="10vh"
  >
    <template #titleRight>
      <span style="float: right">
        <common-button :loading="crud.status.cu === CRUD.STATUS.PROCESSING" size="mini" type="primary" @click="crud.submitCU">
          提 交
        </common-button>
        <store-operation type="crud" />
        <common-button size="mini" @click="crud.cancelCU">关 闭</common-button>
      </span>
    </template>
    <div class="form">
      <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="100px" class="demo-form">
        <el-form-item label="设备编号：" prop="machineNumber">
          <el-input ref="saveTagInput" v-model="form.machineNumber" class="input-underline" placeholder="输入设备编号" />
        </el-form-item>
        <!-- <el-form-item label="机器名称：" prop="machineName">
          <el-input ref="saveTagInput" v-model="form.machineName" placeholder="输入机器名称" class="input-underline" />
        </el-form-item> -->
        <el-form-item label="设备类型：" prop="machineType" >
             <common-select
            ref="machineTypeRef"
            type="enum"
            v-model="form.machineType"
            :options="machineTypeEnum.ENUM"
            clearable
            placeholder="请选择设备类型"
            style="width: 270px"
            class="filter-item"
            @change='handleChange'
          />
        </el-form-item>
        <el-form-item label="设备名称：" prop="machineName">
          <el-input ref="saveTagInput" v-model="form.machineName" class="input-underline" placeholder="输入设备名称"/>
        </el-form-item>
        <el-form-item label="设备品牌：" prop="brand">
          <el-input ref="saveTagInput" v-model="form.brand" class="input-underline" placeholder="输入设备品牌"/>
        </el-form-item>
        <el-form-item label="所属工厂：" prop="factoryId">
          <!-- <el-select ref="saveTagInput" v-model="form.factory"  placeholder="输入工厂" /> -->
          <factory-select
          ref="factoryRef"
          v-model="form.factoryId"
          placeholder="请选择工厂"
          style="width: 270px"
          @change="factoryChange"
         />
          <!-- <el-input ref="saveTagInput" v-model="form.factory" class="input-underline" placeholder="输入工厂" /> -->
        </el-form-item>
        <el-form-item  label="所属车间：" prop="workshopInfId">
          <!-- <el-select ref="saveTagInput" v-model="form.workshopInf"  placeholder="输入车间" /> -->
          <!-- <workshop-select
          :disabled="isEdit"
          v-model="form.workshopInf"
          :factory-id="form.factoryId"
          placeholder="请先选择车间"
          style="width: 270px"
        /> -->
          <workshop-select
          ref="workshopInfRef"
          v-model="form.workshopInfId"
          placeholder="请先选择车间"
          :factory-id="form.factoryId"
          style="width: 270px"
          defaultValue
        />
          <!-- <el-input ref="saveTagInput" v-model="form.workshopInf" class="input-underline" placeholder="输入车间" /> -->
        </el-form-item>
        <el-form-item  label="生产线：" prop="productionLineId">
           <production-line-select
           ref="productionLineRef"
          v-model="form.productionLineId"
          :factory-id="form.factoryId"
          placeholder="请先选择生产线"
          style="width: 270px"
          defaultValue>
        </production-line-select>
          <!-- <el-input ref="saveTagInput" v-model="form.productionLine" class="input-underline" placeholder="输入生产线" /> -->
        </el-form-item>
        <!-- <el-form-item label="生产线类型：" prop="productType">
               <template v-slot="scope">
          <table-cell-tag
            v-if="scope.row.productType"
            :name="componentTypeEnum.V[scope.row.productType].SL"
            :color="componentTypeEnum.V[scope.row.productType].COLOR"
            :offset="15"
          />
          </template>
        </el-form-item> -->
        <el-form-item label="负责人：" prop="directorId">
          <user-select
            ref="userRef"
            v-model="form.directorId"
            placeholder="请选择负责人"
            style="width: 270px"
            defaultValue />
          <!-- <el-input ref="saveTagInput" v-model="form.director" class="input-underline" placeholder="输入负责人" /> -->
        </el-form-item>
        <el-form-item label="MAC地址：" prop="mac">
          <el-input ref="saveTagInput" :disabled="isEdit" v-model="form.mac" class="input-underline" placeholder="输入MAC地址" />
        </el-form-item>
         <el-form-item label="工控机地址" prop="opcUrl">
          <el-input ref="saveTagInput" v-model="form.opcUrl" class="input-underline" placeholder="输入工控机地址" />
        </el-form-item>
        <!-- <el-form-item label="代号：" prop="code">
          <el-input ref="saveTagInput" v-model="form.code" class="input-underline" placeholder="输入代号" />
        </el-form-item> -->
      </el-form>
    </div>
  </common-dialog>
</template>

<script setup>
import { ref, computed } from 'vue'
import { regForm } from '@compos/use-crud'
import { machineTypeEnum } from '@enum-ms/cutting'
import storeOperation from '@crud/STORE.operation'
import factorySelect from '@comp-base/factory-select.vue'
import workshopSelect from '@comp-mes/workshop-select'
import productionLineSelect from '@comp-mes/production-line-select'
// import useOnlyProductLines from '@compos/store/use-only-product-lines'
import userSelect from '@comp-common/user-select'
// import { componentTypeEnum } from '@enum-ms/mes'
// import workshop from '@/api/mes/production-config/workshop'

const formRef = ref()
// 工厂信息
const factoryRef = ref()
// 车间信息
const workshopInfRef = ref()
// 生产线信息
const productionLineRef = ref()
// 人员信息
const userRef = ref()

const defaultForm = {
  machineNumber: '', // 机器编号
  machineName: '', // 机器名称
  machineType: '', // 机器类型
  workshopInfId: '', // 车间信息
  // position: '', // 位置
  factoryId: '', // 工厂
  productionLineId: '', // 生产线
  directorId: '', // 负责人
  opcUrl: '', // 工控机地址
  brand: '品牌', // 设备品牌
  mac: '' // 设备mac地址
  // code: ''
}
const isEdit = computed(() => crud.status.edit >= 1)
const { crud, form, CRUD } = regForm(defaultForm, formRef)

const rules = {
  machineNumber: [{ required: true, message: '请输入设备编号', trigger: 'blur' }],
  machineName: [{ required: true, message: '请输入设备名称', trigger: 'blur' }],
  machineType: [{ required: true, message: '请选择设备类型', trigger: 'blur' }],

  factoryId: [{ required: true, message: '请选择工厂', trigger: 'blur' }],
  productionLineId: [{ required: true, message: '请选择生产线', trigger: 'blur' }],
  workshopInfId: [{ required: true, message: '请选择车间', trigger: 'blur' }],
  mac: [{ required: true, message: '请输入MAC地址', trigger: 'blur' }],
  opcUrl: [{ required: true, message: '请输入工控机地址', trigger: 'blur' }],
  directorId: [{ required: true, message: '请选择负责人', trigger: 'blur' }]
}

// const defaultQuery = {
//   machineType: undefined,
// }
function handleChange(val) {
  console.log(val)
}

function factoryChange() {
  crud.form.workshopInfId = undefined
  crud.form.productionLineId = undefined
}
// 编辑之前
CRUD.HOOK.afterToEdit = (crud, form) => {
  console.log(form)
}

// 处理刷新数据
CRUD.HOOK.beforeToQuery = async () => { }
// 编辑之前
CRUD.HOOK.beforeToEdit = () => {
  console.log(form)
}

// 提交前
CRUD.HOOK.beforeSubmit = async () => {
  console.log(factoryRef.value.factories)
  // 工厂信息
  form.factory = factoryRef.value.getOption(form.factoryId).name
  // return false

  // 车间信息
  console.log(workshopInfRef.value.workshops)
  form.workshopInf = workshopInfRef.value.getOption(form.workshopInfId).name

  // 生产线信息
  console.log(productionLineRef.value.onlyProductLines)
  form.productionLine = productionLineRef.value.getOption(form.productionLineId).name
  // 人员信息
  console.log(userRef.value.users)
  form.director = userRef.value.users.find(v => v.id === form.directorId)?.name

  // 设备类型信息
  // console.log(machineTypeRef.value.MachineTypeEnum.ENUM,'111111111')
  // return false
//   form.machineType = machineTypeRef.value.MachineTypeEnum.ENUM.find(v=>{v.id===form.})
}
</script>

<style lang="scss" scoped></style>
