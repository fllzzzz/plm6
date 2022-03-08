<template>
  <div class="head-container">
    <div class="filter-container">
      <div class="filter-left-box">
        <el-input
          v-model="query.name"
          placeholder="可输入型材名称搜索"
          class="filter-item"
          style="width: 250px"
          size="small"
          clearable
          @keyup.enter="crud.toQuery"
        />
      </div>
      <div class="filter-right-box">
        <el-tag v-permission="permission.edit" v-for="sd in standard" :key="sd.id" size="medium" effect="plain" style="cursor: pointer" @click.self="toSetStandard(sd)">
          {{ sd.name }}
          <i v-permission="permission.del" v-if="sd.deletable" class="el-icon-delete" style="cursor: pointer" @click.self="toDelStandard(sd.id)" />
        </el-tag>
        <common-button v-permission="permission.add" size="mini" type="success" icon="el-icon-plus" @click="crud.toAdd" />
      </div>
    </div>
    <!-- <crudOperation :permission="permission" /> -->
  </div>
</template>

<script setup>
import { batchSetStandard, delStandard } from '@/api/config/classification-manage/section-steel-spec-config'
import { inject, defineEmits } from 'vue'
import { regHeader } from '@compos/use-crud'
import { ElMessage, ElMessageBox } from 'element-plus'
// import crudOperation from '@crud/CRUD.operation'
const defaultQuery = {
  name: undefined
}

const emit = defineEmits(['refresh'])

const { crud, query } = regHeader(defaultQuery)
const standard = inject('standard')
const permission = inject('permission')

// 删除国标
async function toDelStandard(id) {
  ElMessageBox.confirm('此操作将删除该国标, 是否继续?', '提示', { type: 'warning' })
    .then(() => {
      delStandard(id)
        .then(() => {
          emit('refresh')
          ElMessage({ type: 'success', message: '删除成功!' })
        })
        .catch((error) => {
          console.log('删除国标', error)
        })
    })
    .catch(() => {
      ElMessage({ type: 'info', message: '已取消删除' })
    })
}

// 设置国标
function toSetStandard(standard) {
  if (crud.selections.length > 0) {
    ElMessageBox.confirm(`此操作会将所有勾选型材的标准设置为${standard.name}, 确认继续？`, '提示', { type: 'warning' })
      .then(() => {
        const sectionSteelIds = crud.selections
        batchSetStandard(standard.id, sectionSteelIds).then(() => {
          ElMessage({ type: 'success', message: '设置成功!' })
          crud.selectAllChange([])
          emit('refresh')
        })
      })
      .catch(() => {
        ElMessage({ type: 'info', message: '已取消设置' })
      })
  } else {
    ElMessage.warning(`操作会将所有勾选型材的标准设置为${standard.name}，请先勾选型材`)
  }
}
</script>

<style lang="scss" scoped>
.filter-right-box > :nth-child(n) {
  margin-left: 10px;
}
</style>
