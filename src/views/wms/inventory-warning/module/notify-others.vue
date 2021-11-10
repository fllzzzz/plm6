<common-drawer
      v-model="notifyVisible"
      :with-header="true"
      direction="rtl"
      :load-delay="200"
      :show-delay="300"
      title="编辑"
      size="40%"
    >
      <template #title>
        <span class="line-title">
          <span>预警通知人</span>
        </span>
      </template>
      <template #content>
        <!-- <div> -->
        <div style="margin: 30px 20px">
          <el-table :border="$TBS.BORDER" :stripe="$TBS.STRIPE" :data="factoryList" style="width: 100%">
            <el-table-column v-if="$TBS.INDEX" :label="$TBS.INDEX_LABEL ? '序号' : ''" type="index" align="center" width="60" />
            <el-table-column prop="name" label="工厂名称" :show-overflow-tooltip="true" width="150" />
            <el-table-column prop="createTime" label="通知人/部门" :show-overflow-tooltip="true" min-width="180">
              <template v-slot="scope">
                <!-- <div class="flex-rsc"> -->
                <common-radio-button
                  v-model:value="scope.row.userOrDept"
                  :options="userOrDeptEnum"
                  type="enum"
                  size="small"
                  style="margin-right: 10px; margin-bottom: 6px"
                /><br />
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
                <!-- </div> -->
              </template>
            </el-table-column>
            <!-- <el-table-column label="操作" width="130px">
              <template v-slot="scope">
                <el-button v-if="!isShow" type="danger" icon="el-icon-delete" size="mini" @click="toDelete(scope.$index)" />
                <el-button v-show="showDownload && currentUpload.indexOf(scope.row.id) == -1" v-permission="downloadPerm" :loading="downloadLoading" size="mini" type="warning" icon="el-icon-download" @click="downloadFile(scope.$index)" />
              </template>
            </el-table-column> -->
          </el-table>
          <!-- <span style="font-weight:bold">工厂1号</span>
          <common-radio-button
            :value.sync="processStatus"
            :options="processEnum"
            type="enum"
            size="small"
            class="filter-item"
          />
          <el-divider />
          <div class="flex-rss" style="margin-top:20px;margin-bottom:20px">
            <span style="margin-right:10px;">部门</span>
            <div>
              <user-dept-cascader
                :filter-not-dd-user="false"
                :filterable="true"
                :collapse-tags="false"
                multiple
                clearable
                show-all-levels
                style="width: 400px;"
                placeholder="选择通知人"
              />
            </div>
          </div>
          <div class="flex-rss">
            <span style="margin-right:10px;">用户</span>
            <div>
              <user-dept-cascader
                :filter-not-dd-user="false"
                :filterable="true"
                :collapse-tags="false"
                multiple
                clearable
                show-all-levels
                style="width: 400px;"
                placeholder="选择通知人"
              />
            </div>
          </div> -->
        </div>
        <!-- </div> -->
      </template>
    </common-drawer>

<script setup>
import userDeptCascader from '@/views/components/base/user-dept-cascader'
const userOrDeptEnum = {
  USER: { L: '用户', K: 'USER', V: 1 },
  DEPT: { L: '部门', K: 'DEPT', V: 2 }
}
factoryList: [
        {
          name: '一号工厂',
          userOrDept: 1,
          contentIds: [1, 2]
        },
        {
          name: '二号工厂',
          userOrDept: 2,
          contentIds: [1, 2]
        }
      ],
      notifyVisible: false // 预警通知人窗口

</script>
