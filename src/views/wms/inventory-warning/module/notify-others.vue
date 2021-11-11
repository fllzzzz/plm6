<template>
  <common-drawer v-model="notifyVisible" :with-header="true" direction="rtl" :load-delay="200" :show-delay="300" title="编辑" size="40%">
    <template #title>
      <span class="line-title">
        <span>预警通知人</span>
      </span>
    </template>
    <template #content>
      <div style="margin: 30px 20px">
        <common-table :data="factoryList" style="width: 100%">
          <el-table-column label="序号" type="index" align="center" width="60" />
          <el-table-column prop="name" label="工厂名称" :show-overflow-tooltip="true" width="150" />
          <el-table-column prop="createTime" label="通知人" :show-overflow-tooltip="true" min-width="180">
            <template v-slot="scope">
              <user-dept-cascader
                v-if="scope.row.userOrDept === userOrDeptEnum.USER.V"
                :filter-not-dd-user="false"
                :filterable="true"
                :collapse-tags="false"
                multiple
                clearable
                show-all-levels
                style="width: 100%"
                placeholder="选择通知人"
              />
            </template>
          </el-table-column>
          <el-table-column prop="createTime" label="通知部门" :show-overflow-tooltip="true" min-width="180">
            <template v-slot="scope">
              <user-dept-cascader
                v-if="scope.row.userOrDept === userOrDeptEnum.DEPT.V"
                :filter-not-dd-user="false"
                :filterable="true"
                :collapse-tags="false"
                multiple
                clearable
                show-all-levels
                style="width: 100%"
                placeholder="选择通知人"
              />
            </template>
          </el-table-column>
        </common-table>
      </div>
    </template>
  </common-drawer>
</template>

<script setup>
import userDeptCascader from '@/views/components/base/user-dept-cascader'
</script>
